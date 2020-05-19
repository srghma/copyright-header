{-# LANGUAGE OverloadedLists #-}

module CopyrightHeader.FileContentToSplittedFileContent where

import           Protolude

import qualified Data.Text as Text
import qualified Data.List as List
import Data.Text (Text)
import CopyrightHeader.Utils

data SplittedContent = SplittedContent
  { before :: Maybe Text
  , copyright :: Maybe Text
  , after :: Text
  } deriving stock (Generic, Show, Eq)

isCopyrightInTheParagraph :: [Text] -> Text -> Bool
isCopyrightInTheParagraph firstFiveWordsFromTemplate paragraph = Text.words paragraph & List.isInfixOf firstFiveWordsFromTemplate

fileContentToSplittedFileContent :: Text -> Text -> Either Text SplittedContent
fileContentToSplittedFileContent templateWithoutNames fileContent =
  let
    paragraphs' :: [Text] = paragraphs fileContent
    firstFiveWordsFromTemplate = Text.words templateWithoutNames & take 5
    paragraphWithCopyrightIndex :: Maybe Int = List.findIndex (isCopyrightInTheParagraph firstFiveWordsFromTemplate) paragraphs'
   in
    case paragraphWithCopyrightIndex of
      Nothing ->
        Right $ SplittedContent
        { before = Nothing
        , copyright = Nothing
        , after = fileContent
        }
      Just copyrightParagraphIndex ->
        case splitAt copyrightParagraphIndex paragraphs' of
          (headParagraphs, copyright : tail) ->
            Right $ SplittedContent
            { before = if null headParagraphs then Nothing else Just $ unparagraphs headParagraphs
            , copyright = Just $ copyright
            , after = unparagraphs tail
            }
          _ -> Left "Expected to find copyright paragraph"
