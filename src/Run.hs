module Run (run) where

import qualified Database.MongoDB as Mongo
import Import
import Web.Scotty.Trans
import qualified RIO.Text.Lazy as TL

query :: MonadIO m => App -> Mongo.Action m a -> m a
query app = Mongo.access (app ^. _dbPipe) Mongo.master (app ^. _dbName)

run :: RIO App ()
run = do
  app <- ask
  scottyT 3000 (runRIO app) $ do
    get "/blog-posts" $ do
      posts <- query app $ do
        Mongo.rest =<< Mongo.find (Mongo.select [] "blogPosts")
      json $ filter isJust $ fmap (\doc -> fromDoc doc :: Maybe BlogPost) posts
    post "/blog-posts" $ do
      bp <- jsonData :: ActionT TL.Text (RIO App) BlogPostBody
      ret <- query app $ do
        Mongo.insert "blogPosts" $ toDoc bp
      logInfo "blog post inserted"
      json $ show ret
