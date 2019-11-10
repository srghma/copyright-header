module CopyrightHeader where

import           Protolude

import qualified Control.Monad.Catch as Catch
import qualified Data.String                  as String
import Data.String                  (String)
import qualified Data.Text                     as Text
import qualified Text.Read                     as Read
import qualified Data.List.NonEmpty as NonEmpty
import qualified GHC.Show (Show (..))
import qualified Data.List                     as List
import Control.Monad.Fail
import qualified Text.Read (readEither)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH

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

mygroup :: Eq b => (a -> b) -> [a] -> [(b, [a])]
mygroup _ [] = []
mygroup getFieldF (x:xs) = (field, x:xsEqToField) : mygroup getFieldF xs
  where
    field = getFieldF x

    (xsEqToField, xsNotEqToField) = List.span (\a -> getFieldF a == field) xs

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
