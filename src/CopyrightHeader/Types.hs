module CopyrightHeader.Types where

import Dhall (FromDhall, ToDhall)
import System.FilePath.GlobPattern

import Protolude
import Control.Newtype.Generics

newtype Name = Name Text
  deriving stock (Generic, Ord, Eq, Show)
  deriving newtype (ToDhall, FromDhall)

instance Newtype Name

newtype Email = Email Text
  deriving stock (Generic, Ord, Eq, Show)
  deriving newtype (ToDhall, FromDhall)

instance Newtype Email

newtype Year = Year Integer
  deriving stock (Generic, Ord, Eq, Show)
  deriving newtype (ToDhall, FromDhall)

instance Newtype Year

newtype YearSpan = YearSpan Text
  deriving stock (Generic, Ord, Eq, Show)
  deriving newtype (ToDhall, FromDhall)

instance Newtype YearSpan

data DhallConfigContributor = DhallConfigContributor
  { name :: Name
  , yearSpan :: YearSpan
  } deriving stock (Generic, Show, Eq)

instance FromDhall DhallConfigContributor
instance ToDhall DhallConfigContributor

data DhallConfig = DhallConfig
  { template :: [DhallConfigContributor] -> [Text]
  , exclude :: [GlobPattern]
  , include :: [GlobPattern]
  , emailToContributorName :: Map Email (Maybe Name)
  } deriving stock (Generic)

instance FromDhall DhallConfig
