module Main where

import           Protolude                                                               hiding
    ( FilePath
    , catch
    , try
    )

import           Codebreaker.Marker.MarkerException
import           Codebreaker.Marker.Type
import           Codebreaker.Game
import           Codebreaker.Marker
import           Codebreaker.Rules
import           Codebreaker.StatsTable
import           Codebreaker.Utils

import           Cli.AbstractUtils
import           Cli.App
import           Cli.Game
import           Cli.GameLoop
import           Cli.Monads
import           Cli.Types

import qualified System.Console.Haskeline                                               as Haskeline

-- import           Turtle
import qualified Data.Aeson                                                              as Aeson
import qualified Data.ByteString.Lazy.Char8                                              as BS
import qualified Data.List                                                               as List
import           Data.String
import qualified Data.Text                                                               ()
import           "prettyprinter" Data.Text.Prettyprint.Doc
    ( (<+>)
    )
import qualified "prettyprinter" Data.Text.Prettyprint.Doc                               as PP
import qualified "prettyprinter-ansi-terminal" Data.Text.Prettyprint.Doc.Render.Terminal as PP
import qualified System.Directory                                                        as Directory
import qualified System.FilePath                                                         as FilePath
import qualified Control.Monad.Random as Random
import qualified Data.Generics.Product as GLens
import qualified Control.Lens as Lens

mkSearchFunc :: [String] -> String -> [Haskeline.Completion]
mkSearchFunc wordList str =
  map Haskeline.simpleCompletion $ filter (str `isPrefixOf`) wordList

mkSettings :: MonadIO m => [String] -> Haskeline.Settings m
mkSettings wordList = Haskeline.setComplete myCompletitionFunc Haskeline.defaultSettings
 where
  myCompletitionFunc =
    Haskeline.completeWord Nothing " \t" $ return . mkSearchFunc wordList

getAppEnv :: MonadIO m => m AppEnv
getAppEnv = do
  statsDir <- liftIO $ Directory.getXdgDirectory Directory.XdgConfig "codebreaker"
  let statsFilePath = statsDir FilePath.</> "codebreaker.state"
  return $ AppEnv { statsFilePath = statsFilePath
                  , secretLength = 4
                  }

----------

appendStatsToFile :: (MonadIO m, MonadPrint m) => FilePath.FilePath -> CompletedGame -> m ()
appendStatsToFile path completedGame = do
  liftIO $ Directory.createDirectoryIfMissing True (FilePath.takeDirectory path)

  exists <- liftIO $ Directory.doesFileExist path

  if exists
    then do
      maybeParsedStats <- liftIO $ Aeson.decodeFileStrict path
      case maybeParsedStats of
        Just parsedStats -> do
          liftIO $ BS.writeFile path (Aeson.encode (completedGame : parsedStats))
          return ()
        Nothing -> do
          printLine "Invalid stats file, overriding"
          liftIO $ BS.writeFile path (Aeson.encode ([completedGame]))
          return ()
    else do
      printLine "Stats file doest yet exists - creating"
      liftIO $ BS.writeFile path (Aeson.encode ([completedGame]))
      return ()

generateSecret :: Random.MonadRandom m => Int -> m (NonEmpty Int)
generateSecret len = do
  x <- Random.getRandomR fromTo
  xs <- take (len - 1) <$> Random.getRandomRs fromTo
  return (x :| xs)
  where
  fromTo = (1, 4)

---------------------------------------------------

processGame :: Difficulty -> Text -> Int -> IO GameResult
processGame difficultyVal username secretLength = do
  secret <- generateSecret secretLength
  putText ("Secret is " <> showT secret)
  putText "Enter guess (4 numbers) or 'hint' or 'exit' (type tab to see all available commands)"
  let gameEnv = GameEnv
        { secret       = secret
        , username     = username
        , difficulty   = difficultyVal
        }
  let gameState = GameState
        { attemptsUsed = 0
        , secretIndexesAlreadyShownAsHint = []
        }

  interpret gameEnv gameState gameLoop
 where
   interpret :: GameEnv -> GameState -> Game GameResult -> IO GameResult
   interpret gameEnv gameState computation = do
     -- (gameResult, _outGameState) <- runStateT (runReaderT (Haskeline.runInputT (mkSettings wordList) computation) gameEnv) gameState
     (gameResult, _outGameState) <- runStateT (runReaderT (computation & Haskeline.runInputT (mkSettings wordList)) gameEnv) gameState
     return gameResult

   wordList :: IsString s => [s]
   wordList = ["hint", "exit"]


-- TODO: lenses inside MonadState https://stackoverflow.com/questions/39184607/when-should-i-use-monadstate-lens-combinators
-- TODO: sync with state file asyncly
main :: IO ()
main = do
  appEnv <- getAppEnv
  interpret appEnv loop
  -- _ $ Haskeline.runInputT (mkSettings wordList) loop
 where
   interpret :: AppEnv -> App () -> IO ()
   interpret appEnv computation = runReaderT (Haskeline.runInputT (mkSettings wordList) computation) appEnv

   wordList :: IsString s => [s]
   wordList = ["start", "rules", "stats", "exit"]

   loopGameProcessor :: (MonadReader AppEnv m, MonadIO m, MonadPrint m, MonadGetInputLine m) => Text -> m ()
   loopGameProcessor username = do
     difficulty <- askDifficutly
     printLine ("Difficulty is " <> showT difficulty)
     printLine "Game is started."
     secretLength' <- reader $ GLens.getField @"secretLength"
     result <- liftIO $ processGame difficulty username secretLength' -- TODO: remove MonadIO, refactor
     case result of
       Exit -> return ()
       Failure -> tryAgain
       Success completedGame -> do
         statsFilePathVal <- reader statsFilePath
         appendStatsToFile statsFilePathVal completedGame
         tryAgain
      where
        tryAgain = do
          printLine "Try again?"
          yn <- askYesNo
          if yn
            then loopGameProcessor username
            else do
              printLine "Bye"
              return ()


   -- TODO: remove MonadIO from loop
   loop :: (MonadReader AppEnv m, MonadIO m, MonadPrint m, MonadGetInputLine m) => m ()
   loop = do
     minput <- getInputLine "% "
     case minput of
       Just "exit"  -> return ()
       Just "start" -> do
         username <- askUsername
         printLine ("Username is " <> username)
         loopGameProcessor username

       Just "rules" -> do
         liftIO $ PP.putDoc
           (   rules
           <>  PP.hardline
           <>  "Possible commands:"
           <+> PP.hsep (PP.punctuate "," wordList)
           <>  PP.hardline
           )
         loop
       Just "stats" -> do
         statsFilePathVal <- reader statsFilePath
         exists <- liftIO $ Directory.doesFileExist statsFilePathVal

         if exists
           then do
             maybeParsedStats <- liftIO $ Aeson.decodeFileStrict statsFilePathVal
             maybe
               (printLine "Invalid stats file")
               (\stats -> if null stats
                 then printLine "Stats are empty"
                 else liftIO . putStr $ statsTable stats
               )
               maybeParsedStats
             loop
           else do
             printLine "Stats is empty"
             loop
         loop
       _ -> do
         liftIO
           . PP.putDoc
           . mconcat
           . List.intersperse PP.hardline
           $ [ PP.annotate
               (PP.color PP.Red)
               "You have passed unexpected command. Please choose one from listed commands"
             , "Possible commands:" <+> PP.fillSep (PP.punctuate "," wordList)
             , PP.hardline
             ]
         loop

