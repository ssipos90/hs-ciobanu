module Run (run) where

import qualified Database.MongoDB as Mongo
import Import
import qualified Web.Scotty.Trans as ST
import qualified RIO.Text.Lazy as TL

query :: MonadIO m => App -> Mongo.Action m a -> m a
query app = Mongo.access (app ^. _pipe) Mongo.master (app ^. _dbName)

blogPostsIndexAction :: (ST.ScottyError e, MonadIO m) => App -> ST.ActionT e m ()
blogPostsIndexAction app = do
  posts <- query app $ do
     Mongo.rest =<< Mongo.find (Mongo.select [] "blogPosts")
  ST.json $ filter isJust $ fmap (\doc -> fromDoc doc :: Maybe BlogPost) posts

run :: RIO App ()
run = do
  app <- ask
  ST.scottyT 3000 (runRIO app) $ do
    ST.get "/blog-posts" $ blogPostsIndexAction app
    ST.post "/blog-posts" $ do
      bp <- ST.jsonData :: ST.ActionT TL.Text (RIO App) BlogPost
      ret <- query app $ do
        Mongo.insert "blogPosts" $ toDoc bp
      logInfo "blog post inserted"
      ST.json $ show ret
