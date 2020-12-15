{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run, Environment (..)) where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.=))
-- import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import Import
import qualified Web.Scotty as S

blogPostsIndexAction :: Config -> S.ActionM ()
blogPostsIndexAction c = do
  bpds <- Mongo.access (c ^. _pipe) Mongo.master "catalog" $ do
    Mongo.rest =<< Mongo.find (Mongo.select [] "products")
  S.json $ filter isJust $ fmap (\doc -> fromDoc doc :: Maybe BlogPost) bpds

-- blogPostsCreateAction :: Config -> S.ActionM ()
-- blogPostsCreateAction c = do

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
  App {config = c} <- ask
  logDebug "asd"
  liftIO $ do
    S.scotty 3000 $ do
      S.get "/blog-posts" $ blogPostsIndexAction c
      S.post "/blog-posts" $ do
        payload <- S.jsonData :: S.ActionM BlogPost
        asd <- Mongo.access (c ^. _pipe) Mongo.master "catalog" $ do
          Mongo.insert "products" $ toDoc payload
        S.json $ show asd
