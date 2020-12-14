{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Import
import qualified Web.Scotty as S
import qualified Database.MongoDB as Mongo
import RIO.Lens (make)

data WordWrap = WordWrap
  { word :: Text,
    timestamp :: Int
  }
  deriving (Generic)

instance ToJSON WordWrap

instance FromJSON WordWrap

data Config = Config
  { environment :: Environment,
    pipe :: Mongo.Pipe
  }

makeLensesFor'' [("pipe", "_pipe")] Config

homeAction :: RIO App (S.ActionM ())
homeAction = do
  app <- ask
  products <- Mongo.access (app ^. mongoPipe) Mongo.master "catalog" $ do
    Mongo.rest =<< Mongo.find "products" 
  return $ liftIO $ S.json (products :: [Mongo.Document])

  --  e <- Mongo.access pipe Mongo.master "baseball" $ do
  --   clearTeams
  --   insertTeams
  --   allTeams >>= printDocs "All Teams"
  --   nationalLeagueTeams >>= printDocs "National League Teams"
  --   newYorkTeams >>= printDocs "New York Teams"
  --  close pipe
  --  print e

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  homeAction' <- homeAction
  liftIO $ S.scotty 3000 $ do
            S.get "/" homeAction'
