module Main where

import           Protolude                                                               hiding
    ( FilePath
    , catch
    , try
    )

import qualified Data.List                                                               as List
import qualified Data.List.NonEmpty                                                               as NonEmpty
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified System.Directory                                                        as Directory
import qualified System.FilePath                                                         as FilePath
import Options.Applicative
import qualified Dhall
import qualified Turtle
import System.FilePath.GlobPattern
import qualified Data.Text.ANSI as ANSI
import qualified Control.Newtype.Generics as Newtype

import           CopyrightHeader.LanguageTypes
import           CopyrightHeader.Types
import qualified CopyrightHeader.Language      as CopyrightHeader.Language
import qualified CopyrightHeader.Utils         as CopyrightHeader.Utils
import qualified CopyrightHeader.Comment       as CopyrightHeader.Comment
import qualified CopyrightHeader.HistoryToDhallConfigContributors
                                               as CopyrightHeader.HistoryToDhallConfigContributors
import qualified CopyrightHeader.FileContentToSplittedFileContent
                                               as CopyrightHeader.FileContentToSplittedFileContent
import           CopyrightHeader.FileContentToSplittedFileContent
                                                ( SplittedContent(..) )

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
fileComment =
  CopyrightHeader.Language.getCommentStyle
    . CopyrightHeader.Language.getLangFromExt
    . FilePath.takeExtension

-- TODO: http://hackage.haskell.org/package/pipes-async-0.1.3/docs/Pipes-Async.html
main :: IO ()
main = do
  (config :: ConfigOptions) <- execParser opts

  let configFilePath_ = configFilePath config
  configFilePathAbsolute <- Directory.makeAbsolute configFilePath_
  configFilePathExists <- Directory.doesFileExist configFilePathAbsolute
  unless configFilePathExists (Turtle.die $ "File " <> toS configFilePathAbsolute <> " does not exists")

  let rootDirectoryPath_ = rootDirectoryPath config
  rootDirectoryPathAbsolute <- Directory.makeAbsolute rootDirectoryPath_
  rootDirectoryPathExists <- Directory.doesDirectoryExist rootDirectoryPathAbsolute
  unless rootDirectoryPathExists (Turtle.die $ "Directory " <> toS rootDirectoryPathAbsolute <> " does not exists")

  -- TODO: may fail, NonEmpty.fromList Raises an error if given an empty list
  (gitTrackedFiles :: [FilePath.FilePath]) <- fmap (fmap toS . Text.splitOn "\n") . Turtle.strict $ Turtle.inproc "git" ["ls-files"] empty

  dhallConfig :: DhallConfig <- Dhall.input Dhall.auto (toS configFilePathAbsolute)
  let templateFn :: [DhallConfigContributor] -> [Text] = CopyrightHeader.Types.template dhallConfig
  let templateWithoutNames :: Text                     = templateFn [] & Text.concat
  let excludePatterns :: [GlobPattern]                 = CopyrightHeader.Types.exclude dhallConfig
  let includePatterns :: [GlobPattern]                 = CopyrightHeader.Types.include dhallConfig
  let emailToContributorName :: Map Email (Maybe Name) = CopyrightHeader.Types.emailToContributorName dhallConfig

  let gitTrackedFiles_ :: [FilePath.FilePath] = do
        file <- gitTrackedFiles
        guard $ file /= ""
        guard $ any (file ~~) includePatterns
        guard $ all (file /~) excludePatterns
        return file

  let gitTrackedFilePathsWithComment :: [(FilePath.FilePath, Comment)] = fmap (\f -> (f, fileComment f)) gitTrackedFiles_

  let unknownFilePaths :: [FilePath.FilePath] = fst <$> List.filter ((== Comment [] []) . snd) gitTrackedFilePathsWithComment

  when (unknownFilePaths /= []) (Turtle.die $ "Unknown file extensions " <> show unknownFilePaths)

  -- TODO: https://hackage.haskell.org/package/pipes-text-0.0.2.5/docs/Pipes-Text-IO.html
  forM_ gitTrackedFilePathsWithComment (\(filePath, commentStyle) -> do
    (history :: Text) <- Turtle.strict $ Turtle.inproc "git" ["log", "--encoding=utf-8", "--full-history", "--reverse", "--format=format:%at;%aE", toS filePath] empty

    case CopyrightHeader.HistoryToDhallConfigContributors.historyToDhallConfigContributors emailToContributorName history of
      Left (CopyrightHeader.HistoryToDhallConfigContributors.UnknownContributorsError emails) ->
        let
          printedEmails = emails
            & NonEmpty.toList
            & map (\x -> "  " <> Newtype.unpack x) & Text.intercalate "\n"
         in putStrLn $ toS filePath <> ": " <> ANSI.red ("Unknown emails:\n" <> printedEmails)
      Left (CopyrightHeader.HistoryToDhallConfigContributors.UnexpectedError errorMessage) -> Turtle.die $ "Error for " <> toS filePath <> ": " <> show errorMessage
      Right inputs -> do
        (fileContent :: Text) <- Text.readFile filePath

        SplittedContent { before, copyright, after } <- CopyrightHeader.FileContentToSplittedFileContent.fileContentToSplittedFileContent templateWithoutNames fileContent
          & either (\errorMessage -> Turtle.die $ "Error for " <> toS filePath <> ": " <> show errorMessage) pure

        newCopyright :: [Text] <- templateFn (NonEmpty.toList inputs)
            & CopyrightHeader.Utils.wrapLines 100
            & CopyrightHeader.Comment.comment commentStyle
            & map (map Text.strip)
            & either (\errorMessage -> Turtle.die $ "Error for " <> toS filePath <> ": " <> show errorMessage) pure -- TODO: should not happen actually

        let newContent :: Text = maybe
                  (CopyrightHeader.Utils.unparagraphs [CopyrightHeader.Utils.unlinesWithoutNewlineOnEnd newCopyright, after])
                  (\before' -> CopyrightHeader.Utils.unparagraphs [before', CopyrightHeader.Utils.unlinesWithoutNewlineOnEnd newCopyright, after])
                  before

        if fileContent == newContent
           then putStrLn $ toS filePath <> ": " <> ANSI.green "copyright is up to date"
           else do
            case copyright of
              Just _ -> putStrLn $ toS filePath <> ": " <> ANSI.yellow "copyright was updated"
              Nothing -> putStrLn $ toS filePath <> ": " <> ANSI.yellow "copyright was added"

            Text.writeFile filePath newContent
    )
