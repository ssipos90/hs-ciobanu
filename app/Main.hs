{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Import
import Run
import RIO.Process
import qualified Database.MongoDB as Mongo

getMongoPipe :: IO Mongo.Pipe
getMongoPipe = do
  p <- Mongo.connect (Mongo.host "127.0.0.1")
  Mongo.access p Mongo.master "admin" (Mongo.auth "play" "1q2w3e") >>= \case
    False -> exitWith $ ExitFailure 2
    True -> return p

main :: IO ()
main = do
  lo <- logOptionsHandle stdout True
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf -> do
    p <- liftIO getMongoPipe
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , pipe = p
          , dbName = "playCiobanu"
          }
     in runRIO app run
