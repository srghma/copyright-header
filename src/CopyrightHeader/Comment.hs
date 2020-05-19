module CopyrightHeader.Comment where

import Protolude

import qualified Data.Text as Text
import CopyrightHeader.LanguageTypes
import CopyrightHeader.Language

-- single line comment style is preferred
comment :: Comment -> [Text] -> Either Text [Text]
comment (Comment single multi) lines =
  case head single of
    Just singleComment -> Right $ map (\x -> singleComment <> " " <> x) lines
    Nothing ->
      case head multi of
        Just (multiStart, multiEnd) -> Right $ [multiStart] <> lines <> [multiEnd]
        Nothing -> Left "Expected to find some comment styling, but it was empty"
