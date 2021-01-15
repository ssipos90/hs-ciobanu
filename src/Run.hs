{-# LANGUAGE LambdaCase #-}

module Run (run) where

import           Data.Aeson                  (object, (.=))
import           Data.Bson                   (ObjectId (..))
import           Database.MongoDB            ((=:))
import qualified Database.MongoDB            as Mongo
import           Import
import           Network.HTTP.Types          (status500)
import           Network.Wai.Middleware.Cors (simpleCors)
import qualified RIO.Text.Lazy               as TL
import           Web.Scotty.Trans

query :: MonadIO m => App -> Mongo.Action m a -> m a
query app = Mongo.access (app ^. _dbPipe) Mongo.master (app ^. _dbName)

docToBlogPost :: Mongo.Document -> Maybe BlogPost
docToBlogPost doc = fromDoc doc :: Maybe BlogPost

blogPostList :: ActionT TL.Text (RIO App) ()
blogPostList = do
  app <- ask
  docs <- query app (Mongo.rest =<< Mongo.find (Mongo.select [] "blogPosts"))
  let posts = docToBlogPost <$> docs
  if all isJust posts
    then json posts
    else do
      status status500
      json ("Failed to do shit" :: Text)

blogPostShow :: ActionT TL.Text (RIO App) ()
blogPostShow = do
  app <- ask
  postIdStr <- param "postId"
  case readEither postIdStr :: Either TL.Text ObjectId of
    Left e -> raise e
    Right postId -> do
      doc <- docToBlogPost <$> query app (Mongo.fetch (Mongo.select ["_id" =: postId ] "blogPosts"))
      case doc of
        Just bp -> json bp
        Nothing -> do
            status status500
            json ("Failed to do shit" :: Text)

blogPostCreate :: ActionT TL.Text (RIO App) ()
blogPostCreate = do
  app <- ask
  bpb <- jsonData :: ActionT TL.Text (RIO App) BlogPostBody
  bp <- query app $ do
    _id <- Mongo.insert "blogPosts" $ toDoc bpb
    Mongo.fetch (Mongo.select ["_id" =: _id] "blogPosts")
  logInfo "blog post inserted"
  json $ docToBlogPost bp

run :: RIO App ()
run = do
  app <- ask
  scottyT 3000 (runRIO app) $ do
    middleware simpleCors
    matchAny "/test" $ do
      json ("test" :: Text)
    get "/blog-posts" blogPostList
    get "/blog-posts/:postId" blogPostShow
    post "/blog-posts" blogPostCreate
