module Run (run) where

import Data.Bson (ObjectId(..))
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import Import
import Web.Scotty.Trans
import Network.HTTP.Types (status404)
import qualified RIO.Text.Lazy as TL
import Network.Wai.Middleware.Cors (simpleCors)

query :: MonadIO m => App -> Mongo.Action m a -> m a
query app = Mongo.access (app ^. _dbPipe) Mongo.master (app ^. _dbName)

run :: RIO App ()
run = do
  app <- ask
  scottyT 3000 (runRIO app) $ do
    middleware simpleCors
    get "/blog-posts" $ do
      posts <- query app $ do
        Mongo.rest =<< Mongo.find (Mongo.select [] "blogPosts")
      json $ filter isJust $ fmap (\doc -> fromDoc doc :: Maybe BlogPost) posts
    get "/blog-posts/:postId" $ do
      postIdStr <- param "postId"
      case readEither postIdStr :: Either TL.Text ObjectId of
        Left e -> raise e
        Right postId -> do
          maybeDoc <- query app $ do
            Mongo.findOne (Mongo.select ["_id" =: postId ] "blogPosts")
          case maybeDoc of
            Just doc -> do
              case fromDoc doc :: Maybe BlogPost of
                Just bp -> json bp
                Nothing -> json ("asdas" :: Text)
            Nothing -> do
              status status404
              json ("Post not found" :: Text)
    post "/blog-posts" $ do
      bp <- jsonData :: ActionT TL.Text (RIO App) BlogPostBody
      _id <- query app $ do
        Mongo.insert "blogPosts" $ toDoc bp
      logInfo "blog post inserted"
      json $ show _id
