module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_peasant
import qualified Database.MongoDB as Mongo
import System.Environment (lookupEnv)
import RIO.Partial (read)

getEnvironment :: IO Environment
getEnvironment =
  fmap
    (maybe Development read)
    (lookupEnv "SCOTTY_ENV")

getMongoPipe :: Environment -> IO Mongo.Pipe
getMongoPipe _ = Mongo.connect (Mongo.host "127.0.0.1")

getConfig :: IO Config
getConfig = do
  e <- getEnvironment
  p <- getMongoPipe e
  return Config
      { environment = e,
        pipe = p
      }


main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_peasant.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  config <- getConfig
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , config = config
          }
     in runRIO app run
