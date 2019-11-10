{-# LANGUAGE OverloadedLists #-}

module HistoryToInputsSpec where

import           Protolude

import           Test.Hspec

import qualified Data.Text as Text
import Data.Text (Text)
import Data.String.QQ

import           CopyrightHeader.HistoryToDhallConfigTemplateInputs
import           CopyrightHeader.Types

spec :: Spec
spec = do
  it "1" $ do
    let gitLogOutput :: Text = [s|
1406873814;User1 user
1537187208;srghma
|]
    let expected :: NonEmpty DhallConfigTemplateInput =
          [
            (DhallConfigTemplateInput "User1 user" "2014")
          , (DhallConfigTemplateInput "srghma" "2018")
          ]
    historyToDhallConfigTemplateInputs gitLogOutput `shouldBe` Right expected
  it "2" $ do
    let gitLogOutput = [s|
1406873814;User1 user
1406873815;User1 user
1537187208;srghma
|]
    let expected :: NonEmpty DhallConfigTemplateInput =
          [
            (DhallConfigTemplateInput "User1 user" "2014")
          , (DhallConfigTemplateInput "srghma" "2018")
          ]
    historyToDhallConfigTemplateInputs gitLogOutput `shouldBe` Right expected
  it "3" $ do
    let gitLogOutput = [s|
1406873814;User1 user
1537187208;srghma
1573325887;srghma
|]
    let expected :: NonEmpty DhallConfigTemplateInput =
          [
            (DhallConfigTemplateInput "User1 user" "2014")
          , (DhallConfigTemplateInput "srghma" "2018-2019")
          ]
    historyToDhallConfigTemplateInputs gitLogOutput `shouldBe` Right expected
