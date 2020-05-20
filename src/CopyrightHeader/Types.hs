module CopyrightHeader.Types where

import Dhall (FromDhall, ToDhall)
import System.FilePath.GlobPattern

import Protolude
import Control.Newtype.Generics

newtype Name = Name Text deriving stock (Generic, Ord, Eq, Show)

instance Newtype Name
instance FromDhall Name
instance ToDhall Name

newtype Email = Email Text deriving stock (Generic, Ord, Eq, Show)

instance Newtype Email
instance FromDhall Email
instance ToDhall Email

newtype Year = Year Integer deriving stock (Generic, Ord, Eq, Show)

instance Newtype Year
instance FromDhall Year
instance ToDhall Year

newtype YearSpan = YearSpan Text deriving stock (Generic, Ord, Eq, Show)

instance Newtype YearSpan
instance FromDhall YearSpan
instance ToDhall YearSpan

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
  , emailToContributorName :: Map Email Name
  } deriving stock (Generic)

instance FromDhall DhallConfig
