{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Shared.Types where

import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Database.Persist
import           Database.Persist.Sql (fromSqlKey, SqlBackend)
import           Database.Persist.TH ( mkMigrate, mkPersist, persistLowerCase
                                     , share, sqlSettings
                                     )
import qualified Prelude

import           Protolude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  name Text
  email Text
  deriving Eq Show

Rsvp sql=rsvps
  event_id EventId
  name Text
  contact Text
  deriving Eq Show

EventRsvps sql=event_rsvps
  event_id EventId
  rsvp_id RsvpId
  deriving Eq Show

Event sql=events
  creator_id UserId
  name Text
  contact Text
  timeStart UTCTime Maybe
  timeEnd UTCTime Maybe
  image ByteString Maybe
  deriving Eq Show
|]

$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Rsvp)
$(deriveJSON defaultOptions ''EventRsvps)

type RsvpEvent = Event
type DbKey = Int64


encodeToText :: ByteString -> Text
encodeToText = T.decodeUtf8 . B64.encode

decodeFromText :: (Monad m) => Text -> m ByteString
decodeFromText t = case B64.decode $ T.encodeUtf8 t of
  Left a -> Prelude.error a
  Right bs -> pure bs

-- $(deriveJSON defaultOptions ''Event)
instance ToJSON Event where
  toJSON (Event c name contact s e image)
    = object [ "creator_id" .= toJSON c
             , "name" .= toJSON name
             , "contact" .= toJSON contact
             , "start_time" .= s
             , "end_time" .= e
             , "image" .= fmap encodeToText image
             ]
instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "creator_id"
                               <*> v .: "name"
                               <*> v .: "contact"
                               <*> v .:? "start_time"
                               <*> v .:? "end_time"
                               <*> ((v .: "image") >>= pure . decodeFromText)
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
type EventsResponse = PaginatedResponse Event
type UsersResponse = PaginatedResponse User
type RsvpsResponse = PaginatedResponse Rsvp

newtype EventCreateResponse =
  EventCreateResponse (CreateResponse Event)
  deriving (Eq, Show, Generic)

instance FromJSON EventCreateResponse
instance ToJSON EventCreateResponse

-- instance (ToJSON a, PersistEntity a, FromJSON a) => ToJSON (PaginatedResponse a) where
--   toJSON (PaginatedResponse page per_page ls) = object [ "page" .= toJSON page
--                                                        , "per_page" .= toJSON per_page
--                                                        , "content" .= toJSON ls ]

-- instance (FromJSON a, PersistEntity a, ToJSON a) => FromJSON (PaginatedResponse a) where
--   parseJSON (Object v) = PaginatedResponse <$> v .: "page"
--                                            <*> v .: "per_page"
--                                            <*> v .: "content"
--   parseJSON x = typeMismatch "List Response" x

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
