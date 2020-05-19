module Main where

import           Protolude                                                               hiding
    ( FilePath
    , catch
    , try
    )

import qualified Data.Aeson                                                              as Aeson
import qualified Data.ByteString.Lazy.Char8                                              as BS
import qualified Data.List                                                               as List
import qualified Data.List.NonEmpty                                                               as NonEmpty
import           Data.String
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           "prettyprinter" Data.Text.Prettyprint.Doc
    ( (<+>)
    )
import qualified "prettyprinter" Data.Text.Prettyprint.Doc                               as PP
import qualified System.Directory                                                        as Directory
import qualified System.FilePath                                                         as FilePath
import qualified Data.Generics.Product as GLens
import qualified Control.Lens as Lens
import Options.Applicative
import Data.Semigroup ((<>))
import qualified Dhall
import qualified Turtle
import Dhall (FromDhall, ToDhall, Natural)
import System.FilePath.GlobPattern

import CopyrightHeader.LanguageTypes
import CopyrightHeader.Language
import CopyrightHeader.Types
import CopyrightHeader.Utils
import CopyrightHeader.Comment
import CopyrightHeader.HistoryToDhallConfigTemplateInputs
import CopyrightHeader.FileContentToSplittedFileContent

data ConfigOptions = ConfigOptions
  { configFilePath :: FilePath.FilePath
  , rootDirectoryPath :: FilePath.FilePath
  }
  deriving stock (Show)

configOptionsParser :: Parser ConfigOptions
configOptionsParser = ConfigOptions
      <$> strOption
          (  long "config"
          <> short 'c'
          <> metavar "FILENAME"
          <> showDefault
          <> value "./.copyright-header.dhall"
          <> help "Yaml config file" )
      <*> strOption
          (  long "root"
          <> short 'r'
          <> metavar "DIR"
          <> showDefault
          <> value "./."
          <> help "Root directory" )

opts :: ParserInfo ConfigOptions
opts = info (configOptionsParser <**> helper)
  ( fullDesc
 <> progDesc "Add copyright header"
 <> header "copyright-header" )

fileComment :: FilePath.FilePath -> Comment
fileComment = getCommentStyle . getLangFromExt . FilePath.takeExtension

-- TODO: http://hackage.haskell.org/package/pipes-async-0.1.3/docs/Pipes-Async.html
main :: IO ()
main = do
  (config :: ConfigOptions) <- execParser opts

  let configFilePath_ = configFilePath config
  configFilePathAbsolute <- Directory.makeAbsolute configFilePath_
  configFilePathExists <- Directory.doesFileExist configFilePathAbsolute
  unless configFilePathExists (die $ "File " <> toS configFilePathAbsolute <> " does not exists")

  let rootDirectoryPath_ = rootDirectoryPath config
  rootDirectoryPathAbsolute <- Directory.makeAbsolute rootDirectoryPath_
  rootDirectoryPathExists <- Directory.doesDirectoryExist rootDirectoryPathAbsolute
  unless rootDirectoryPathExists (die $ "Directory " <> toS rootDirectoryPathAbsolute <> " does not exists")

  -- TODO: may fail, NonEmpty.fromList Raises an error if given an empty list
  (gitTrackedFiles :: [FilePath.FilePath]) <- fmap (fmap toS . Text.splitOn "\n") . Turtle.strict $ Turtle.inproc "git" ["ls-files"] empty

  dhallConfig :: DhallConfig <- Dhall.input Dhall.auto (toS configFilePathAbsolute)
  let templateFn :: [DhallConfigTemplateInput] -> [Text] = template dhallConfig
  let templateWithoutNames :: Text = templateFn [] & Text.concat
  let excludePatterns :: [GlobPattern] = exclude dhallConfig
  let includePatterns :: [GlobPattern] = include dhallConfig

  -- putStrLn (show excludePatterns :: String)

  -- putStrLn (show gitTrackedFiles :: String)

  let gitTrackedFiles_ :: [FilePath.FilePath] = do
        file <- gitTrackedFiles
        guard $ file /= ""
        guard $ any (file ~~) includePatterns
        guard $ all (file /~) excludePatterns
        return file

  -- putStrLn (show gitTrackedFiles_ :: String)

  let gitTrackedFilePathsWithComment :: [(FilePath.FilePath, Comment)] = fmap (\f -> (f, fileComment f)) gitTrackedFiles_

  let unknownFilePaths :: [FilePath.FilePath] = fst <$> List.filter ((== Comment [] []) . snd) gitTrackedFilePathsWithComment

  when (unknownFilePaths /= []) (Turtle.die $ "Unknown file extensions " <> show unknownFilePaths)

  -- TODO: https://hackage.haskell.org/package/pipes-text-0.0.2.5/docs/Pipes-Text-IO.html
  forM_ gitTrackedFilePathsWithComment (\(filePath, commentStyle) -> do
    (history :: Text) <- Turtle.strict $ Turtle.inproc "git" ["log", "--encoding=utf-8", "--full-history", "--reverse", "--format=format:%at;%an", toS filePath] empty

    inputs <- historyToDhallConfigTemplateInputs history
      & either (\errorMessage -> Turtle.die $ "Error for " <> toS filePath <> ": " <> show errorMessage) pure

    (fileContent :: Text) <- Text.readFile filePath

    (SplittedContent { before, copyright, after }) <- fileContentToSplittedFileContent templateWithoutNames fileContent
      & either (\errorMessage -> Turtle.die $ "Error for " <> toS filePath <> ": " <> show errorMessage) pure

    newCopyright :: [Text] <- templateFn (NonEmpty.toList inputs)
        & wrapLines 80
        & comment commentStyle
        & either (\errorMessage -> Turtle.die $ "Error for " <> toS filePath <> ": " <> show errorMessage) pure -- TODO: should not happen actually

    -- putText (show $ Text.unlines newCopyright)

    let unlinesWithoutNewlineOnEnd = Text.intercalate "\n"

    let newContent :: Text = maybe (unparagraphs [unlinesWithoutNewlineOnEnd newCopyright, after]) (\before' -> unparagraphs [before', unlinesWithoutNewlineOnEnd newCopyright, after]) before

    case copyright of
      Just _ -> putStrLn $ filePath <> ": copyright was updated"
      Nothing -> putStrLn $ filePath <> ": copyright was added"

    Text.writeFile filePath newContent
    )
