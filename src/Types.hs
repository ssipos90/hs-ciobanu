{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON, withObject, (.:), (.=))
import qualified Data.Bson as Bson
import Database.MongoDB ((=:))
import qualified Database.MongoDB as Mongo
import Lens.Micro.TH (makeLensesFor)
import RIO
import RIO.Process

data Options = Options
  { optionsVerbose :: !Bool,
    optionsGTFO :: !Bool
  }

data BlogPost = BlogPost
  { _id :: Bson.ObjectId,
    title :: Text,
    content :: Text
  }

class ToMongoIO a where
  toDoc :: a -> Mongo.Document

class FromMongoIO a where
  fromDoc :: Mongo.Document -> Maybe a

makeLensesFor
  [ ("title", "_title"),
    ("content", "_content")
  ]
  ''BlogPost

data BlogPostBody = BlogPostBody
  { title :: Text,
    content :: Text
  }

instance FromJSON BlogPostBody where
  parseJSON = withObject "BlogPost" $ \v ->
    BlogPostBody
      <$> v .: "title"
      <*> v .: "content"

instance ToJSON BlogPost where
  toJSON BlogPost {_id, title, content} =
    object
      [ "_id" .= show _id,
        "title" .= title,
        "content" .= content
      ]

instance ToMongoIO BlogPost where
  toDoc BlogPost {_id, title, content} =
    [ "_id" =: _id,
      "title" =: title,
      "content" =: content
    ]

instance FromMongoIO BlogPost where
  fromDoc d =
    BlogPost
      <$> Mongo.lookup "_id" d
      <*> Mongo.lookup "title" d
      <*> Mongo.lookup "content" d

instance ToMongoIO BlogPostBody where
  toDoc BlogPostBody {title, content} =
    [ 
      "title" =: title,
      "content" =: content
    ]

data App = App
  { appLogFunc :: !LogFunc,
    appProcessContext :: !ProcessContext,
    dbPipe :: Mongo.Pipe,
    dbName :: Text
  }

makeLensesFor
  [ ("dbPipe", "_dbPipe"),
    ("dbName", "_dbName"),
    ("environment", "_environment")
  ]
  ''App

instance HasLogFunc App where
  logFuncL = lens appLogFunc (\x y -> x {appLogFunc = y})

instance HasProcessContext App where
  processContextL = lens appProcessContext (\x y -> x {appProcessContext = y})
