{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RankNTypes #-}
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

import Data.Binary              (Binary)
import Data.ByteString (ByteString)
import Data.ByteString.Base64   as Base64
import Data.Data                (Data, Typeable)
import Data.Hashable            (Hashable)
import Data.Serialize           (Serialize)
import Data.String              (IsString (..))
import GHC.Generics             (Generic)
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Database.Persist
import           Database.Persist.Sql (fromSqlKey, SqlBackend)
import qualified Prelude

import           Protolude
import Database.Persist.TH (
  mkMigrate, mkPersist, persistLowerCase,
  share, sqlSettings)

-- | Aeson serialisable bytestring. Uses base64 encoding.
newtype ByteString64 = ByteString64 { getByteString64 :: ByteString }
    deriving (Eq, Read, Show, Ord, Data, Typeable, Generic, Hashable, Serialize, Binary)

-- | Get base64 encode bytestring
getEncodedByteString64 :: ByteString64 -> ByteString
getEncodedByteString64 = Base64.encode . getByteString64

instance ToJSON ByteString64 where
    toJSON = toJSON . T.decodeLatin1 . getEncodedByteString64

instance FromJSON ByteString64 where
    parseJSON = withText "ByteString" $
        pure . ByteString64 . decodeLenient . encodeUtf8

instance IsString ByteString64 where
   fromString = ByteString64 . fromString

instance NFData ByteString64

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
    image ByteString Maybe
    deriving Eq Show
|]

encodeToText :: ByteString -> Text
encodeToText = T.decodeUtf8 . B64.encode

decodeFromText :: (Monad m) => Text -> m ByteString
decodeFromText = either Prelude.fail return . B64.decode . T.encodeUtf8

-- $(deriveJSON defaultOptions ''Event)
$(deriveJSON defaultOptions ''User)
$(deriveJSON defaultOptions ''Rsvp)
$(deriveJSON defaultOptions ''EventRsvps)

type RsvpEvent = Event
type DbKey = Int64

instance ToJSON Event where
  toJSON (Event c name contact image) = object [ "creator_id" .= toJSON c
                                               , "name" .= toJSON name
                                               , "contact" .= toJSON contact
                                               , "image" .= fmap encodeToText image
                                               ]
instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "creator_id"
                               <*> v .: "name"
                               <*> v .: "contact"
                               <*> pure (Just "image") -- ((v .: "image") >>= decodeFromText)
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

data ListResponse a
  = ListResponse
  { _content :: [Entity a]
  } deriving (Generic)

type EventsResponse = ListResponse Event
type UsersResponse = ListResponse User
type RsvpsResponse = ListResponse Rsvp

newtype EventCreateResponse =
  EventCreateResponse (CreateResponse Event)
  deriving (Eq, Show, Generic)

instance FromJSON EventCreateResponse
instance ToJSON EventCreateResponse

instance (ToJSON a, PersistEntity a) => ToJSON (ListResponse a) where
  toJSON (ListResponse ls) = object [ "content" .= toJSON ls ]

instance (FromJSON a, PersistEntity a) => FromJSON (ListResponse a) where
  parseJSON (Object v) = ListResponse <$> v .: "content"
  parseJSON x = typeMismatch "List Response" x

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
