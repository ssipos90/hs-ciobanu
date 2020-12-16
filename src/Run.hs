module Run (run, Environment (..)) where

import qualified Database.MongoDB as Mongo
import Import
import qualified Web.Scotty as S

blogPostsIndexAction :: Config -> S.ActionM ()
blogPostsIndexAction c = do
  bpds <- Mongo.access (c ^. _pipe) Mongo.master "playCiobanu" $ do
    Mongo.rest =<< Mongo.find (Mongo.select [] "blogPosts")
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

suckIt :: MonadIO m => Mongo.Pipe -> Mongo.Action m a -> m a
suckIt p = Mongo.access p Mongo.master "playCiobanu"

run :: RIO App ()
run = do
  logInfo "We're inside the application!"
  App {config = c} <- ask
  liftIO $ do
    S.scotty 3000 $ do
      S.get "/blog-posts" $ blogPostsIndexAction c
      S.post "/blog-posts" $ do
        bp <- S.jsonData :: S.ActionM BlogPost
        ret <- suckIt (c ^. _pipe) $ do
          Mongo.insert "products" $ toDoc bp
        S.json $ show ret
