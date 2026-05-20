{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Export.HTML.Common (renderToText) where

import "this" Prelude

import Data.FileEmbed
import Data.Text.Encoding qualified as T
import Data.Text.Lazy     qualified as T
import Lucid


renderToText :: Html () -> Text
renderToText body = T.toStrict $ Lucid.renderText $ doctypehtml_ do
  style_ $ T.decodeUtf8Lenient css
  script_ $ T.decodeUtf8Lenient script
  body_ body
  where
    css = $(embedFileRelative "lib/Lang/Pietre/Export/HTML/Resources/style.css")
    script = $(embedFileRelative "lib/Lang/Pietre/Export/HTML/Resources/script.js")
