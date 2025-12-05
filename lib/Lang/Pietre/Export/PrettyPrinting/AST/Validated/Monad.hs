{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Export.PrettyPrinting.AST.Validated.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict qualified as M
import Data.Text           qualified as T


type Printer = State PrinterState

data PrinterState = PrinterState
  { _psIDs     :: HashMap Text Text
  , _psCounter :: Int
  }

runPrinter :: Printer a -> a
runPrinter = flip evalState $ PrinterState M.empty 0

makeLenses ''PrinterState

register :: Text -> Printer Text
register name = do
  counter <- use psCounter
  psCounter += 1
  let uniqueID = name <> "-" <> T.pack (show counter)
  psIDs %= M.insert name uniqueID
  pure uniqueID

retrieve :: Text -> Printer Text
retrieve name = do
  uses psIDs (M.lookup name) `onNothingM`
    register name

nested :: Printer a -> Printer a
nested action = do
  oldIDs <- use psIDs
  result <- action
  psIDs .= oldIDs
  pure result
