{- | GramViz

This standalone program parses an input grammar file, in this project's specific
PEG format (see [docs/pietre.gram]) and applies the given command to it.
-}

module Main where

import "this" Prelude

import Control.Lens               hiding ((...))
import Control.Monad.Extra        (whenM)
import Control.Monad.RWS
import Data.Char
import Data.HashMap.Strict        qualified as M
import Data.HashSet               qualified as S
import Data.List                  (nub)
import Data.Maybe                 (fromJust)
import Data.Text                  qualified as T
import Data.Text.IO               qualified as T
import System.Environment
import System.Exit
import System.IO                  qualified as IO
import Text.Builder               qualified as TB
import Text.Megaparsec            hiding (Token, token, tokens)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Printf


--------------------------------------------------------------------------------
-- main

help :: IO ()
help = do
  putStrLn "usage:\
    \\n\
    \\n    gramviz command [args]\
    \\n\
    \\ncommands:\
    \\n    (help      | h )                 display this help\
    \\n    (dot       | d ) grammar_file    parses the grammar, prints a DOT representation\
    \\n    (tree      | t ) grammar_file    parses the grammar, prints a tree representation\
    \\n    (terminals | ts) grammar_file    parses the grammar, prints a list of all terminals"

main :: IO ()
main = getArgs >>= \case
  ["help"] -> help
  ["h"]    -> help
  ["dot",       g] -> retrieve g >>= dot
  ["d",         g] -> retrieve g >>= dot
  ["tree",      g] -> retrieve g >>= tree
  ["t",         g] -> retrieve g >>= tree
  ["terminals", g] -> retrieve g >>= terminals
  ["ts",        g] -> retrieve g >>= terminals
  _ -> help >> exitFailure

retrieve :: String -> IO Grammar
retrieve filename = do
  source <- T.readFile filename
  parseGrammar filename source `onLeft` \e -> do
    putStrLn e
    exitFailure


--------------------------------------------------------------------------------
-- parsing

type Grammar = RuleTree

data RuleTree = RuleTree Rules Sections

type Sections = [(Text, RuleTree)]
type Rules    = [(Text, Expr)]

type Expr = [[QuantitativeTerm]]

data QuantitativeTerm = QTerm Term Modifier

data Modifier
  = Single
  | Optional
  | Many
  | Some

data Term
  = Identifier Text
  | String     Text
  | Group      Expr

parseGrammar :: FilePath -> Text -> Either String Grammar
parseGrammar = left errorBundlePretty ... parse (ruleTree 0 <* eof)
  where
    ruleTree depth = RuleTree
      <$> many rule
      <*> many (section (depth+1))

    rule = do
      name <- identifier
      symbol ":"
      expr <- expression
      pure (name, expr)

    section depth = do
      try $ count depth (char '#')
      too_deep <|> continue depth

    too_deep = do
      char '#'
      unexpected $ Tokens $ pure '#'

    continue depth = do
      hspace1
      name <- printChar `manyTill` newline
      space1
      sub  <- ruleTree depth
      pure (T.pack name, sub)

    expression = some term `sepBy1` symbol "|"

    term = do
      value :: Term <- choice
        [ Identifier <$> try (identifier <* notFollowedBy (symbol ":"))
        , String     <$> stringLit
        , Group      <$> parens
        ]
      modifier <- choice
        [ Many     <$ symbol "*"
        , Some     <$ symbol "+"
        , Optional <$ symbol "?"
        , pure Single
        ]
      pure $ QTerm value modifier

    identifier = fmap T.pack $ lexeme $ some (alphaNumChar <|> char '_')
    stringLit  = fmap T.pack $ lexeme $ between (char '"') (char '"') $ some $ satisfy (/= '"')
    parens     = between (symbol "(") (symbol ")") expression


type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = L.space space1 empty empty

symbol :: Text -> Parser Text
symbol = L.symbol whitespace

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace


--------------------------------------------------------------------------------
-- tree

tree :: Grammar -> IO ()
tree gram = render . map snd =<< withTTYInfo (displayTree gram)
  where
    displayTree (RuleTree rules sections) = do
      r <- traverse displayRule rules
      s <- traverse displaySection sections
      pure $ map (True,) r <> concatMap (zip (True : repeat False)) s

    displayRule (name, _) = pure $ TB.text name

    displaySection (name, ruleTree) = do
      header  <- applyCode "1;36m" (T.unpack name)
      content <- displayTree ruleTree
      let (withoutLines, withLines) = each %~ reverse $ break fst $ reverse content
      pure $ TB.string header : indent withLines <> map (mappend "    " . snd) withoutLines

    indent = \case
      []                 -> []
      ((_,     line):[]) -> mappend "└── " line : []
      ((True,  line):ls) -> mappend "├── " line : indent ls
      ((False, line):ls) -> mappend "│   " line : indent ls


--------------------------------------------------------------------------------
-- terminals

terminals :: Grammar -> IO ()
terminals gram = render =<< withTTYInfo do
  h1 <- applyCode "1;36m" "non-specific tokens"
  h2 <- applyCode "1;36m" "keywords"
  h3 <- applyCode "1;36m" "operators"
  (identifiers, keywords, operators, rules) <- execStateT (visitTree gram) ([], [], [], S.empty)
  let tokens = filter (not . (`S.member` rules)) identifiers
  pure $ concat
    [ [TB.string h1]
    , map (mappend "    " . TB.text) (nub $ reverse tokens)
    , ["", TB.string h2]
    , map (mappend "    " . TB.text) (nub $ reverse keywords)
    , ["", TB.string h3]
    , map (mappend "    " . TB.text) (nub $ reverse operators)
    ]
  where
    visitTree (RuleTree rules sections) = do
      traverse_ visitRule rules
      traverse_ visitSection sections

    visitRule (name, expr) = do
      _4 %= S.insert name
      visitExpr expr

    visitSection =
      visitTree . snd

    visitExpr =
      traverse_ visitTerm . concatMap (map getTerm)

    visitTerm = \case
      Group  expr -> visitExpr expr
      Identifier name -> _1 %= (name:)
      String text ->
        if T.all isAlphaNum text
          then _2 %= (text:)
          else _3 %= (text:)

    getTerm (QTerm term _) = term



--------------------------------------------------------------------------------
-- dot

dot :: Grammar -> IO ()
dot gram = render =<< do
  let (content, (_, paths), nodes) =
        runRWS (visitTree gram) [] (0 :: Int, M.empty)
      alterations = do
        (nodeID, nodeRule) <- nodes
        guard $ not $ M.member nodeRule paths
        pure $ mkToken nodeID
  pure $ concat
    [ fileHeader
    , indent <$> content
    , indent <$> alterations
    , fileFooter
    ]
  where
    indent = ("  " <>)

    fileHeader =
      [ "digraph grammar {"
      , "  compound=true;"
      ]

    fileFooter =
      [ "}"
      ]

    visitTree (RuleTree rules sections) =
      liftA2 (concat ... mappend)
        (traverse visitRule rules)
        (traverse visitSection sections)

    visitSection (sectionName, ruleTree) = do
      clusterID <- use _1
      _1 += 1
      path <- ask
      let subgraphName = T.pack $ printf "s%04d" clusterID
          bgColor = case length path of
            0 -> "#E4F1EE"
            1 -> "#D9EdF8"
            2 -> "#DEDAF4"
            _ -> error "too deep"
          config =
            [ "bgcolor=\"" <> bgColor <> "\";"
            , "labeljust=\"l\";"
            , "fontsize=20;"
            , "fontname=\"Helvetica-Oblique\";"
            ]
      content <- local (sectionName:) $ visitTree ruleTree
      pure $ mkSubgraph subgraphName sectionName $ config <> content

    visitRule (name, expr) = do
      path <- ask
      whenM (uses _2 $ M.member name) $
        error $ "multiple definitions for " ++ T.unpack name
      _2 %= M.insert name path
      let start = name <> "_START"
          end   = name <> "_END"
          ((edges, _, ends), (_, localNodes), textNodes) =
            runRWS (visitExpr [start] expr) (path, name) (0, [])
      tell textNodes
      pure $ mkSubgraph name name $ concat
        [ [ TB.text start <> " [label=\"\", fixedsize=\"true\", width=0.2, shape=\"circle\"]"
          , TB.text end   <> " [label=\"\", fixedsize=\"true\", width=0.2, shape=\"doublecircle\"]"
          , "bgcolor=\"white\""
          , "labeljust=\"c\";"
          , "fontsize=16;"
          , "fontname=\"Helvetica\";"
          ]
        , localNodes
        , edges
        , map (`mkLink` end) ends
        ]

    visitExpr prevs expr = do
      (edges, starts, ends) <- unzip3 <$> traverse (visitBranch prevs [] Nothing) expr
      pure ( concat edges
           , nub $ concat starts
           , nub $ concat ends
           )

    visitBranch prevs res starts = \case
      []     -> pure (res, fromJust starts, prevs)
      (q:qs) -> do
        (newEdges, newStarts, newPrevs) <- visitQualifiedTerm prevs q
        visitBranch newPrevs (res <> newEdges) (starts <|> Just newStarts) qs

    visitQualifiedTerm prevs (QTerm t m) = do
      (l, s, e) <- visitTerm prevs t
      let loop = liftA2 mkLink e s
      pure $ case m of
        Single   -> (        l, s, e)
        Optional -> (        l, s, e <> prevs)
        Some     -> (loop <> l, s, e)
        Many     -> (loop <> l, s, e <> prevs)

    visitTerm prevs = \case
      Identifier desc -> do
        name <- declareNode mkNode desc
        tell $ pure (name, desc)
        pure (map (`mkLink` name) prevs, [name], [name])
      String desc -> do
        name <-
          if T.all isAlphaNum desc
            then declareNode mkKeyword  desc
            else declareNode mkOperator desc
        pure (map (`mkLink` name) prevs, [name], [name])
      Group expr -> visitExpr prevs expr

    declareNode f desc = do
      rule <- asks snd
      (current, register) <- get
      let name = rule <> "_" <> T.pack (printf "%08d" (current :: Int))
      put (current + 1, register <> [f name desc])
      pure name

    mkSubgraph :: Text -> Text -> [TB.Builder] -> [TB.Builder]
    mkSubgraph name desc content = concat
      [ [ "subgraph cluster_" <> TB.text name <> " {"
        , "  label=\"" <> TB.text desc <> "\";"
        ]
      , indent <$> content
      , [ "}"
        ]
      ]

    mkNode name desc =
      TB.text name <> " [label=\"" <> TB.text desc <> "\"]"

    mkLink a b =
      TB.text a <> " -> " <> TB.text b

    mkToken name =
      TB.text name <> " [shape=\"box\", style=\"filled\", fillcolor=\"#FFAEAE\"]"

    mkKeyword name desc =
      TB.text name <> " [label=\"" <> TB.text desc <> "\", style=\"filled\", shape=\"box\", fillcolor=\"#FFD7A6\"]"

    mkOperator name desc =
      TB.text name <> " [label=\"" <> TB.text desc <> "\", style=\"filled\", shape=\"diamond\", fillcolor=\"#FDFFB6\"]"


--------------------------------------------------------------------------------
-- print

newtype TTYInfo = TTYInfo Bool

withTTYInfo :: MonadIO m => ReaderT TTYInfo m a -> m a
withTTYInfo action = getTTYInfo >>= runReaderT action
  where
    getTTYInfo = TTYInfo <$> liftIO (IO.hIsTerminalDevice IO.stdout)

applyCode
  :: (MonadReader TTYInfo m)
  => String
  -> String
  -> m String
applyCode code str = do
  TTYInfo isTTY <- ask
  pure $ if isTTY then printf "\ESC[%s%s\ESC[0m" code str else str

render :: [TB.Builder] -> IO ()
render = TB.putLnToStdOut . TB.intercalate "\n"
