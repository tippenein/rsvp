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

import Protolude
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

type Status = Either Text Text

newtype EventCreateResponse =
  EventCreateResponse Status
  deriving (Eq, Show, Generic)

instance FromJSON EventCreateResponse where
  parseJSON (Object v) = EventCreateResponse <$> v .: "status"
  parseJSON x = typeMismatch "Events" x


instance ToJSON EventCreateResponse where
  toJSON (EventCreateResponse event) = object ["event" .= toJSON event]
