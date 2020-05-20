module CopyrightHeader.HistoryToDhallConfigContributors where

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import qualified Text.Read
import Data.Time.Calendar(toGregorian)
import Data.Time (Day)
import Data.Time.Clock(utctDay)
import Data.Time.Clock.POSIX(posixSecondsToUTCTime)
import qualified Control.Newtype.Generics as Newtype
import qualified Data.List.NonEmpty.Singleton as NonEmpty

import Protolude
import CopyrightHeader.Types

data GitHistoryRow = GitHistoryRow
  { email :: Email
  , year :: Year
  }

data HistoryToDhallConfigContributorsError
  = UnknownContributorsError (NonEmpty Email)
  | UnexpectedError Text
  deriving stock (Generic, Ord, Eq, Show)

historyToDhallConfigContributors :: Map Email (Maybe Name) -> Text -> Either HistoryToDhallConfigContributorsError (NonEmpty DhallConfigContributor)
historyToDhallConfigContributors emailToContributorName history = do
  let history' :: [[Text]] =
        history
        & Text.splitOn "\n"
        & List.filter (/= "")
        & fmap (Text.splitOn ";")

  (history'' :: NonEmpty [Text]) <- nonEmpty history' & note (UnexpectedError "Expected git history to contain at least 1 row")

  (history''' :: NonEmpty GitHistoryRow) <- traverse toGitHistoryRow history'' & first UnexpectedError

  let history_map :: Map Email [Year] =
        history'''
        & NonEmpty.toList
        & fmap (\GitHistoryRow { email, year } -> (email, year))
        & convertKVList

  let
    emailToName :: Email -> Either (NonEmpty Email) (Maybe Name)
    emailToName email = Map.lookup email emailToContributorName & note (NonEmpty.singleton email)

  let constributors :: Either (NonEmpty Email) [(Maybe Name, [Year])] =
        history_map
        & Map.toList
        & fmap (first emailToName)
        & map (\(errorOrName, years) -> either Left (\name -> Right (name, years)) errorOrName)
        & collectErrors

  (constributors' :: [(Maybe Name, [Year])]) <-
        constributors
        & first UnknownContributorsError

  let contributors'' :: Map Name [Year] =
        constributors'
        & map (\(maybeName, years) -> maybeName >>= \name -> Just (name, years))
        & catMaybes
        & Map.fromListWith (++)

  (contributors''' :: NonEmpty (Name, [Year])) <-
        contributors''
        & Map.toList
        & nonEmpty
        & note (UnexpectedError "Expected git history to contain at least 1 row after processing")

  Right (contributors''' <&> (\(name, years) -> DhallConfigContributor { name, yearSpan = yearsToYearSpan years }))

yearsToYearSpan :: [Year] -> YearSpan
yearsToYearSpan years = year
  where
    years' :: [Integer] = fmap Newtype.unpack years
    min_ :: Integer = List.minimum years'
    max_ :: Integer = List.maximum years'
    year = if min_ == max_ then YearSpan (show min_) else YearSpan (show min_ <> "-" <> show max_)

toGitHistoryRow :: [Text] -> Either Text GitHistoryRow
toGitHistoryRow [timestamp, email] = do
  timestamp' :: Int <- first toS $ Text.Read.readEither (toS timestamp)
  let day :: Day = unixEpochInSecondsToDay timestamp'
  let (year, _month, _day) = toGregorian day
  return (GitHistoryRow { email = Email email, year = Year year })
toGitHistoryRow other = throwError $ "Expected to find [timestamp, email], but received " <> show other

-- https://stackoverflow.com/questions/44905138/how-to-convert-epoch-to-gregorian-datetime-in-haskell/49782238
-- https://two-wrongs.com/haskell-time-library-tutorial
unixEpochInSecondsToDay :: Int -> Day
unixEpochInSecondsToDay = utctDay . posixSecondsToUTCTime . fromIntegral

-- https://stackoverflow.com/questions/15514486/haskell-converting-a-list-of-a-b-key-value-pairs-with-possibly-repeated-key
convertKVList :: Ord k => [(k, v)] -> Map k [v]
convertKVList = Map.fromListWith (++) . fmap (second (:[]))

-- gitHistoryRowsToMap :: [GitHistoryRow] -> GitHistoryMap
-- gitHistoryRowsToMap rows =
--   let
--     rows' :: [(Name, ([Email], [Year]))] = fmap (\(GitHistoryRow name email year) -> (name, ([email], [year]))) rows
--   in Map.fromListWith (\(e1, y1) (e2, y2) -> (e1 <> e2, y1 <> y2)) rows'


collectErrors :: Semigroup errors => [Either errors a] -> Either errors [a]
collectErrors [] = Right []
collectErrors (Left errors : xs) = Left $ go errors xs
  where
    go :: Semigroup errors => errors -> [Either errors a] -> errors
    go accum ((Right _):xs') = go accum xs'
    go accum ((Left errors'):xs') = go (accum <> errors') xs'
    go accum [] = accum
collectErrors ((Right i):xs) = fmap (i :) (collectErrors xs)
