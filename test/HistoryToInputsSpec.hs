{-# LANGUAGE OverloadedLists #-}

module HistoryToInputsSpec where

import           Protolude

import           Test.Hspec

import qualified Data.Map as Map
import Data.Text (Text)
import Data.String.QQ

import           CopyrightHeader.HistoryToDhallConfigContributors
import           CopyrightHeader.Types

emailToContributorNameMap :: Map Email (Maybe Name)
emailToContributorNameMap = Map.fromList
  [ (Email "user1@mail.com", Just (Name "User 1"))
  , (Email "srghma@mail.com", Just (Name "Serhii Khoma"))
  , (Email "robot@mail.com", Nothing)
  ]

spec :: Spec
spec = do
  it "different users" $ do
    let gitLogOutput :: Text = [s|
1406873814;user1@mail.com
1537187208;srghma@mail.com
1537187208;robot@mail.com
|]
    let expected :: NonEmpty DhallConfigContributor =
          [ (DhallConfigContributor { yearSpan = (YearSpan "2018"), name = (Name "Serhii Khoma") })
          , (DhallConfigContributor { yearSpan = YearSpan "2014", name = (Name "User 1") })
          ]
    historyToDhallConfigContributors emailToContributorNameMap gitLogOutput `shouldBe` Right expected
  it "one user in same year" $ do
    let gitLogOutput = [s|
1406873814;user1@mail.com
1406873815;user1@mail.com
1537187208;srghma@mail.com
|]
    let expected :: NonEmpty DhallConfigContributor =
          [ (DhallConfigContributor { yearSpan = (YearSpan "2018"), name = (Name "Serhii Khoma") })
          , (DhallConfigContributor { yearSpan = (YearSpan "2014"), name = (Name "User 1") })
          ]
    historyToDhallConfigContributors emailToContributorNameMap gitLogOutput `shouldBe` Right expected
  it "one user in year-span" $ do
    let gitLogOutput = [s|
1406873814;user1@mail.com
1537187208;srghma@mail.com
1573325887;srghma@mail.com
|]
    let expected :: NonEmpty DhallConfigContributor =
          [ (DhallConfigContributor { yearSpan = (YearSpan "2018-2019"), name = (Name "Serhii Khoma") })
          , (DhallConfigContributor { yearSpan = (YearSpan "2014"), name = (Name "User 1") })
          ]
    historyToDhallConfigContributors emailToContributorNameMap gitLogOutput `shouldBe` Right expected
