module CopyrightHeader.HistoryToDhallConfigTemplateInputs where

import qualified Dhall
import qualified Turtle
import Dhall (FromDhall, ToDhall, Natural)
import System.FilePath.GlobPattern
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.List
import Data.List.NonEmpty
import qualified Data.List.NonEmpty
import qualified Data.Map
import Data.Time
import qualified Text.Read
import Data.Time.Calendar(toGregorian)
import Data.Time.Clock(utctDay,UTCTime)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)

import Protolude
import Data.String
import CopyrightHeader.Types
import CopyrightHeader

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
    Just history__ ->
      history__ &
      fmap extractNameAndYear &
      sequence &
      (second $
        (\(x :: NonEmpty (Text, Integer)) ->
          x &
          Data.List.NonEmpty.toList &
          convertKVList &
          fmap (\(name, years) -> DhallConfigTemplateInput name (yearToStr years)) &
          Data.List.NonEmpty.fromList
        )
      )


  where
    yearToStr :: [Integer] -> Text
    yearToStr years = year
      where
        min_ :: Integer = Data.List.minimum years
        max_ :: Integer = Data.List.maximum years
        year = if min_ == max_ then show min_ else show min_ <> "-" <> show max_

    extractNameAndYear :: [Text] -> Either Text (Text, Integer)
    extractNameAndYear [fst, snd] = do
      timestamp :: Int <- first toS $ Text.Read.readEither (toS fst)
      let name = snd
      let day :: Day = unixEpochInSecondsToDay timestamp
      traceShowM day
      let (year, _month, _day) = toGregorian day
      return $ (name, year)
    extractNameAndYear other = throwError $ "Should return 2 elements, but received " <> show other

    -- https://stackoverflow.com/questions/44905138/how-to-convert-epoch-to-gregorian-datetime-in-haskell/49782238
    -- https://two-wrongs.com/haskell-time-library-tutorial
    unixEpochInSecondsToDay :: Int -> Day
    unixEpochInSecondsToDay = utctDay . posixSecondsToUTCTime . fromIntegral
