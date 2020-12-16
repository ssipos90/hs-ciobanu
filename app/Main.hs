{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_peasant

import qualified Database.MongoDB as Mongo
import System.Environment (lookupEnv)

import qualified RIO.ByteString as BS
import qualified RIO.Text as T

getEnvironment :: IO Environment
getEnvironment = do
  lookupEnv "SCOTTY_ENV" >>= \case
    Nothing -> BS.putStr "No env specified\n"
    Just env -> BS.putStr $ T.encodeUtf8 (T.pack env <> " env, however ignoring and setting development\n" :: Text)
  return Development
  -- return $ case env of
  --   Nothing -> Development
  --   Just e -> readEnv e

getMongoPipe :: Environment -> IO Mongo.Pipe
getMongoPipe _ = do
  p <- Mongo.connect (Mongo.host "127.0.0.1")
  isAuthenticated <- Mongo.access p Mongo.master "admin" (Mongo.auth "play" "1q2w3e")
  if isAuthenticated
    then return p
    else exitFailure

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
  c <- getConfig
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          , config = c
          }
     in runRIO app run
