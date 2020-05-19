module CopyrightHeader.Utils where

import           Protolude

import qualified Control.Monad.Catch as Catch
-- import qualified Data.String                  as String
import Data.String                  (String)
import qualified Data.Text                     as Text
import qualified Text.Read                     as Read
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Show (Show (..))
import qualified Data.List                     as List
import Control.Monad.Fail
-- import qualified Text.Read (readEither)

-- import Data.List
-- import Data.Ord
-- import Data.Function (on)

import qualified Data.Map

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import qualified Data.Text.Prettyprint.Doc as Prettyprint
import qualified Data.Text.Prettyprint.Doc.Render.Text as Prettyprint

{-# INLINE count #-}
count :: Eq a => a -> [a] -> Int
count a = length . filter (== a)

stringToIntList :: String -> Maybe [Int]
stringToIntList = traverse charToInt
 where
  charToInt :: Char -> Maybe Int
  charToInt = Read.readMaybe . (: [])

textToIntList :: Text -> Maybe [Int]
textToIntList = stringToIntList . Text.unpack

textToNonEmtpy :: Text -> Maybe (NonEmpty Int)
textToNonEmtpy = fmap NonEmpty.fromList . textToIntList

stringToNonEmtpy :: String -> Maybe (NonEmpty Int)
stringToNonEmtpy = fmap NonEmpty.fromList . stringToIntList

throwIfNothing :: (Catch.MonadThrow m, Exception e) => e -> Maybe a -> m a
throwIfNothing _ (Just a) = return a
throwIfNothing e Nothing  = Catch.throwM e

showT :: (Show a) => a -> Text
showT = Text.pack . GHC.Show.show

traceShowIdWithPrefix :: forall a. Show a => Text -> a -> a
traceShowIdWithPrefix prefix a = trace (prefix <> showT a) a

deleteN :: Int -> [a] -> [a]
deleteN _ []     = []
deleteN !i (a:as)
   | i == 0    = as
   | otherwise = a : deleteN (i-1) as

findIndex :: (a -> Bool) -> NonEmpty a -> Maybe Int
findIndex predicate nonempty = List.findIndex predicate (toList nonempty)

-- https://stackoverflow.com/questions/15514486/haskell-converting-a-list-of-a-b-key-value-pairs-with-possibly-repeated-key
convertKVList :: Ord a => [(a, b)] -> [(a, [b])]
convertKVList = Data.Map.toList . Data.Map.fromListWith (++) . fmap (second (:[]))

-- TODO: wrong
-- groupByAndExtract :: Eq b => (a -> b) -> [a] -> [(b, [a])]
-- groupByAndExtract _ [] = []
-- groupByAndExtract getFieldF (x:xs) = (field, x:xsEqToField) : groupByAndExtract getFieldF xs
--   where
--     field = getFieldF x

--     (xsEqToField, xsNotEqToField) = List.span (\a -> getFieldF a == field) xs

deriving instance Lift a => Lift (NonEmpty a)

-- write `[nonemtpyInt|00000|]`
-- instead of `0 :| [0, 0, 0, 0]`
nonemtpyInt :: QuasiQuoter
nonemtpyInt = QuasiQuoter { quoteExp = makeQuoteExp }
  where
    makeQuoteExp :: String -> Q Exp
    makeQuoteExp val =
      case stringToNonEmtpy val of
        Just nonEmtpyVal -> [| nonEmtpyVal |]
        _ -> fail ("Cant convert to nonEmtpy string: " <> val)

{-
>>> readEither @Text @Int "123"
Right 123
>>> readEither @Text @Int "aa"
Left "Prelude.read: no parse"
-}
-- readEither :: (StringConv a String, Read b) => a -> Either Text b
-- readEither text =
--   let textS :: String = toS text
--    in first toS . Text.Read.readEither $ textS
-- {-# INLINEABLE readEither #-}

paragraphs :: Text -> [Text]
paragraphs = Text.splitOn "\n\n"

unparagraphs :: [Text] -> Text
unparagraphs = Text.intercalate "\n\n"

wrapLines :: Int -> [Text] -> [Text]
wrapLines maxLineWidth lines =
  map Prettyprint.pretty lines
  & Prettyprint.vcat
  & Prettyprint.layoutPretty (Prettyprint.LayoutOptions { layoutPageWidth = Prettyprint.AvailablePerLine maxLineWidth 1.0 })
  & Prettyprint.renderStrict
  & Text.lines
