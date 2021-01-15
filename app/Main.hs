{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import qualified Database.MongoDB as Mongo
import           Import
import           RIO.Process      (mkDefaultProcessContext)
import           Run              (run)

getMongoPipe :: IO Mongo.Pipe
getMongoPipe = do
  p <- Mongo.connect (Mongo.host "127.0.0.1")
  Mongo.access p Mongo.master "admin" (Mongo.auth "play" "1q2w3e") >>= \case
    False -> exitWith $ ExitFailure 2
    True  -> return p

main :: IO ()
main = do
  lo <- logOptionsHandle stdout True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    pipe <- liftIO getMongoPipe
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , dbPipe = pipe
          , dbName = "playCiobanu"
          }
     in runRIO app run
