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

module Shared.Types where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import qualified Prelude

import           Protolude
import Database.Persist.TH (
  mkMigrate, mkPersist, persistLowerCase,
  share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json sql=users
    name Text
    email Text
    deriving Eq Show

Rsvp json sql=rsvps
    event_id EventId
    name Text
    contact Text
    deriving Eq Show

EventRsvps json sql=event_rsvps
    event_id EventId
    rsvp_id RsvpId
    deriving Eq Show

Event json sql=events
    creator_id UserId
    name Text
    contact Text
    deriving Eq Show
|]

type RsvpEvent = Event
type DbKey = Int64

newtype UserResponse =
  UserResponse [User]
  deriving (Eq, Show, Generic)

instance FromJSON UserResponse where
  parseJSON (Object v) = UserResponse <$> v .: "users"
  parseJSON x = typeMismatch "Users" x

instance ToJSON UserResponse where
  toJSON (UserResponse users) = object ["users" .= toJSON users]

newtype RsvpResponse =
  RsvpResponse [Rsvp]
  deriving (Eq, Show, Generic)

instance FromJSON RsvpResponse where
  parseJSON (Object v) = RsvpResponse <$> v .: "rsvps"
  parseJSON x = typeMismatch "Rsvps" x

instance ToJSON RsvpResponse where
  toJSON (RsvpResponse rsvps) = object ["rsvps" .= toJSON rsvps]

newtype EventResponse =
  EventResponse [Event]
  deriving (Eq, Show, Generic)

instance FromJSON EventResponse where
  parseJSON (Object v) = EventResponse <$> v .: "events"
  parseJSON x = typeMismatch "Events" x

instance ToJSON EventResponse where
  toJSON (EventResponse events) = object ["events" .= toJSON events]

newtype EventCreateResponse =
  EventCreateResponse Status
  deriving (Eq, Show, Generic)

instance FromJSON EventCreateResponse where
  parseJSON (Object v) = EventCreateResponse <$> v .: "status"
  parseJSON x = typeMismatch "Events" x

data Status
  = Success Text
  | Warning Text
  | Info Text
  | Error Text
  deriving (Show, Eq, Generic)

instance ToJSON Status
instance FromJSON Status

instance ToJSON EventCreateResponse where
  toJSON (EventCreateResponse status) = object ["status" .= toJSON status]

-- json -----------------------------------------------

aesonDef :: Options
aesonDef = defaultOptions { fieldLabelModifier = dropIdentifier }

-- instance ToJSON (Entity Event) where
--   toJSON (Entity pid (e@Event{..})) = toJSON e <$> object [ "id" .= pid ]

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
