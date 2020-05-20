{-# LANGUAGE OverloadedLists #-}

module FileContentToSplittedFileContentSpec where

import           Protolude

import           Test.Hspec

import Data.Text (Text)
import Data.String.QQ

import           CopyrightHeader.FileContentToSplittedFileContent

-- templateWithoutNames :: Text
-- templateWithoutNames = [s|
-- This file has been authored by

-- (C) copyright by
-- |]

templateWithoutNames :: Text
templateWithoutNames = [s|
© copyright by Trident Sustainability Group, LLC (Green Badger), Savannah, USA
This file has been (co)-authored by
|]

spec :: Spec
spec = do
  describe "without shebang" $ do
    it "without existing header" $ do
      let fileContent :: Text = [s|
def foo
  "bar"
end
|]
      let expected :: SplittedContent =
            SplittedContent
            { before = Nothing
            , copyright = Nothing
            , after = fileContent
            }
      fileContentToSplittedFileContent templateWithoutNames fileContent `shouldBe` Right expected
    it "with existing header" $ do
      let fileContent :: Text = [s|
# © copyright by Trident Sustainability Group, LLC (Green Badger), Savannah, USA
# This file has been (co)-authored by Foo Bar in 2000,
# Baz Bar in 2001

def foo
  "bar"
end
|]
      let expected :: SplittedContent =
            SplittedContent
            { before = Nothing
            , copyright = Just [s|
# © copyright by Trident Sustainability Group, LLC (Green Badger), Savannah, USA
# This file has been (co)-authored by Foo Bar in 2000,
# Baz Bar in 2001|]
            , after = [s|
def foo
  "bar"
end
|]
            }
      fileContentToSplittedFileContent templateWithoutNames fileContent `shouldBe` Right expected
  describe "with shebang" $ do
    it "without existing header" $ do
      let fileContent :: Text = [s|
#!/usr/local/bin/ruby -w

def foo
  "bar"
end
|]
      let expected :: SplittedContent =
            SplittedContent
            { before = Nothing
            , copyright = Nothing
            , after = fileContent
            }
      fileContentToSplittedFileContent templateWithoutNames fileContent `shouldBe` Right expected
    it "with existing header" $ do
      let fileContent :: Text = [s|
#!/usr/local/bin/ruby -w
# some other comment

# © copyright by Trident Sustainability Group, LLC (Green Badger), Savannah, USA
# This file has been (co)-authored by Foo Bar in 2000,
# Baz Bar in 2001

def foo
  "bar"
end
|]
      let expected :: SplittedContent =
            SplittedContent
            { before = Just [s|
#!/usr/local/bin/ruby -w
# some other comment|]
            , copyright = Just [s|
# © copyright by Trident Sustainability Group, LLC (Green Badger), Savannah, USA
# This file has been (co)-authored by Foo Bar in 2000,
# Baz Bar in 2001|]
            , after = [s|
def foo
  "bar"
end
|]
            }
      fileContentToSplittedFileContent templateWithoutNames fileContent `shouldBe` Right expected
