{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.Persist
import           Database.Persist.Sql (fromSqlKey, SqlBackend)
import qualified Prelude

import qualified Shared.Models as Model
import           Protolude

$(deriveJSON defaultOptions ''Model.User)
$(deriveJSON defaultOptions ''Model.Rsvp)
$(deriveJSON defaultOptions ''Model.EventRsvps)

type DbKey = Int64

encodeToText :: ByteString -> Text
encodeToText = T.decodeUtf8 . B64.encode

decodeFromText :: (Monad m) => Maybe Text -> m (Maybe ByteString)
decodeFromText mt = case mt of
  Nothing -> pure Nothing
  Just t -> case B64.decode $ T.encodeUtf8 t of
              Left a -> Prelude.error a
              Right bs -> pure $ Just bs

-- $(deriveJSON defaultOptions ''Event)
instance ToJSON Model.Event where
  toJSON (Model.Event c name contact s e image)
    = object [ "creator_id" .= toJSON c
             , "name" .= toJSON name
             , "contact" .= toJSON contact
             , "start_time" .= s
             , "end_time" .= e
             , "image" .= fmap encodeToText image
             ]

instance FromJSON Model.Event where
  parseJSON (Object v) = do
    mimage <- v .:? "image"
    Model.Event <$> v .: "creator_id"
                <*> v .: "name"
                <*> v .: "contact"
                <*> v .:? "start_time"
                <*> v .:? "end_time"
                <*> decodeFromText mimage
  parseJSON x = typeMismatch "Event" x

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

entityToJSON :: (PersistEntity r, ToJSON r) => Entity r -> Value
entityToJSON (Entity key value) = object
    [ "id" .= key
    , "entity" .= value
    ]

entityFromJSON :: (PersistEntity r, FromJSON r) => Value -> Parser (Entity r)
entityFromJSON (Object o) = Entity <$> o .: "id" <*> o .: "entity"
entityFromJSON x = typeMismatch "entityFromJSON: not an object" x

instance (ToJSON a, PersistEntity a) => ToJSON (Entity a) where
  toJSON = entityToJSON

instance (FromJSON a, PersistEntity a) => FromJSON (Entity a) where
  parseJSON = entityFromJSON

data CreateResponse a
  = CreateResponse
  { _message :: Status
  , _db_id :: DbKey
  , _posted_content :: a
  } deriving (Eq, Show, Generic)

data PaginatedResponse a = PaginatedResponse
  { page :: Int
  , totalPages :: Int
  , _content :: [Entity a]
  } deriving (Generic)

instance (FromJSON a, PersistEntity a) => FromJSON (PaginatedResponse a)
instance (ToJSON a, PersistEntity a) => ToJSON (PaginatedResponse a)
-- Example
type EventsResponse = PaginatedResponse Model.Event
type UsersResponse = PaginatedResponse Model.User
type RsvpsResponse = PaginatedResponse Model.Rsvp

newtype EventCreateResponse =
  EventCreateResponse (CreateResponse Model.Event)
  deriving (Eq, Show, Generic)

instance FromJSON EventCreateResponse
instance ToJSON EventCreateResponse

instance (ToJSON a) => ToJSON (CreateResponse a) where
  toJSON (CreateResponse msg db e) = object [ "content" .= toJSON e
                                            , "db_id" .= toJSON db
                                            , "message" .= toJSON msg
                                            ]
instance (FromJSON a) => FromJSON (CreateResponse a) where
  parseJSON (Object v) = CreateResponse <$> v .: "message"
                                        <*> v .: "db_id"
                                        <*> v .: "content"
  parseJSON x = typeMismatch "CreateResponse" x

data Status
  = Success Text
  | Warning Text
  | Info Text
  | Error Text
  deriving (Show, Eq, Generic)

instance ToJSON Status
instance FromJSON Status


-- json -----------------------------------------------

entityToTuple :: (ToBackendKey SqlBackend a) => Entity a -> (DbKey, a)
entityToTuple e = (fromSqlKey (entityKey e), entityVal e)

aesonDef :: Options
aesonDef = defaultOptions { fieldLabelModifier = dropIdentifier }

-- instance FromJSON ContactInfo where
--   parseJSON = withObject "contact_info" $ \o -> do
--     kind <- o .: "kind"
--     case kind of
--       "phone" -> Phone <$> o .: "phone"
--       "email" -> Email <$> o .: "email"
--       _        -> panic ("unknown contact info: " <> kind)

-- instance ToJSON ContactInfo where
--   toJSON (Phone t) = object [ "phone" .= t]
--   toJSON (Email t) = object [ "email" .= t]

dropIdentifier :: Prelude.String -> Prelude.String
dropIdentifier = T.unpack . T.intercalate "_" . drop 2 . T.splitOn "_" . T.pack
