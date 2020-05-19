module CopyrightHeader.Types where

import qualified Dhall
import qualified Turtle
import Dhall (FromDhall, ToDhall, Natural)
import System.FilePath.GlobPattern
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import Protolude
import Data.String

data DhallConfigTemplateInput = DhallConfigTemplateInput
  {
    name :: Text
  , year :: Text
  } deriving stock (Generic, Show, Eq)

instance FromDhall DhallConfigTemplateInput
instance ToDhall DhallConfigTemplateInput

data DhallConfig = DhallConfig
  {
    template :: [DhallConfigTemplateInput] -> [Text]
  , exclude :: [GlobPattern]
  , include :: [GlobPattern]
  } deriving stock (Generic)

instance FromDhall DhallConfig
