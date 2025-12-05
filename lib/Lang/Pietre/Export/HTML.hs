{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Export.HTML where

import "this" Prelude

import Data.FileEmbed
import Data.Text.Encoding qualified as T
import Data.Text.Lazy     qualified as T
import Lucid


renderHTML :: Html () -> Text
renderHTML body = T.toStrict $ Lucid.renderText $ doctypehtml_ do
  style_ $ T.decodeUtf8Lenient css
  script_ $ T.decodeUtf8Lenient script
  body_ body
  where
    css = $(embedFileRelative "lib/Lang/Pietre/Export/style.css")
    script = $(embedFileRelative "lib/Lang/Pietre/Export/script.js")
