{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Types where

import RIO
import RIO.Process
import qualified Database.MongoDB as Mongo
import Database.MongoDB ((=:))
import Data.Aeson (FromJSON, ToJSON, object, toJSON, parseJSON, withObject, (.=), (.:))
import Lens.Micro.TH (makeLensesFor)

data Options = Options
  { optionsVerbose :: !Bool
  }

data BlogPost = BlogPost
  { title :: Text,
    content :: Text
  }

makeLensesFor [ ("title", "_title")
              , ("content", "_content")
              ] ''BlogPost

class MongoIO a where
  toDoc :: a -> Mongo.Document
  fromDoc :: Mongo.Document -> Maybe a

instance FromJSON BlogPost where
  parseJSON = withObject "BlogPost" $ \v -> BlogPost
   <$> v .: "title"
   <*> v .: "content"

instance ToJSON BlogPost where
  toJSON BlogPost { title = t, content = c} =
    object
      [ "title" .= t,
        "content" .= c
      ]

instance MongoIO BlogPost where
  toDoc p =
    [ "title" =: p ^. _title,
      "content" =: p ^. _content
    ]
  fromDoc d =
    BlogPost
      <$> Mongo.lookup "title" d
      <*> Mongo.lookup "content" d

data App = App
  { appLogFunc :: !LogFunc
  , appProcessContext :: !ProcessContext
  , pipe :: Mongo.Pipe
  , dbName :: Text
  }

makeLensesFor [ ("pipe", "_pipe")
              , ("environment", "_environment")
              , ("dbName", "_dbName")
              ] ''App

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x { appLogFunc = y })

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x { appProcessContext = y })
