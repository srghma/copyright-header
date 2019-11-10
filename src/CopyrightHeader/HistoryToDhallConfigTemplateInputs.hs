module CopyrightHeader.HistoryToDhallConfigTemplateInputs where

import qualified Dhall
import qualified Turtle
import Dhall (FromDhall, ToDhall, Natural)
import System.FilePath.GlobPattern
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List
import Data.List.NonEmpty
import Data.Time
import qualified Text.Read
import Data.Time.Calendar(toGregorian)
import Data.Time.Clock(utctDay,UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)

import Protolude
import Data.String
import CopyrightHeader.Types

historyToDhallConfigTemplateInputs :: Text -> Either Text (NonEmpty DhallConfigTemplateInput)
historyToDhallConfigTemplateInputs history = do
  let history_ :: [[Text]] =
        history &
        Text.splitOn "\n" &
        Data.List.filter (/= "") &
        fmap (Text.splitOn ";")

  traceShowM history_

  case nonEmpty history_ of
    Nothing -> throwError $ "Empty history " <> show history
    Just history__ -> history__ & fmap fn & sequence

  where
    -- https://stackoverflow.com/questions/44905138/how-to-convert-epoch-to-gregorian-datetime-in-haskell/49782238
    -- https://two-wrongs.com/haskell-time-library-tutorial
    fn :: [Text] -> Either Text DhallConfigTemplateInput
    fn [fst, snd] = do
      timestamp :: Int <- first toS $ Text.Read.readEither (toS fst)
      let name = snd
      let day :: Day = utctDay . posixSecondsToUTCTime . fromIntegral $ timestamp
      traceShowM day
      let (year, _month, _day) = toGregorian day
      return $ DhallConfigTemplateInput name (show year)
    fn other = throwError $ "Should return 2 elements, but received " <> show other
