{-# LANGUAGE DeriveGeneric #-}

-- | Shared response types
module Rsvp.Response where

import           Data.Aeson
import           Data.Aeson.Types

import Rsvp.Server.Models

import           Protolude

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
