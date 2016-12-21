{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for rsvp.
module Rsvp.API where

import Protolude

import Data.Aeson
       (FromJSON(..), ToJSON(..), Value(..), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import qualified NeatInterpolation as NI
import Servant.API ((:>), ReqBody,(:<|>)(..), Get, Post, JSON, MimeRender(..))

import Rsvp.API.Internal (HTML)

type API = Get '[HTML] RootPage :<|>
  ListUsers :<|>
  ListRsvps :<|>
  ListEvents :<|>
  CreateEvent

type ListUsers = "users" :> Get '[JSON] UserResponse
type ListRsvps = "rsvps" :> Get '[JSON] RsvpResponse
type ListEvents = "events" :> Get '[JSON] EventResponse
type CreateEvent =
  "events"
  :> ReqBody '[JSON] Event
  :> Post '[JSON] EventResponse

api :: Proxy API
api = Proxy

data User = User
  { _userId :: Int
  , _userFirstName :: Text
  , _userLastName :: Text
  } deriving (Eq, Show, Generic)

data ContactInfo
  = Phone Text
  | Email Text
  deriving (Eq, Show, Generic)

data Rsvp
  = Rsvp
  { _rsvp_event_id :: Int
  , _rsvp_name :: Text
  , _rsvp_contact :: ContactInfo
  } deriving (Eq, Show, Generic)

data Event = Event
  { _event_creator_id :: Int -- User id
  , _event_creator_contact :: ContactInfo
  , _event_name:: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

instance FromJSON Rsvp
instance ToJSON Rsvp

instance FromJSON Event
instance ToJSON Event

instance FromJSON ContactInfo
instance ToJSON ContactInfo

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

data RootPage =
  RootPage

instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
         <!doctype html>
         <html>
         <head><title>rsvp</title></head>
         <body>
         <h1>rsvp</h1>
         <ul>
         <li><a href="/users">users</a></li>
         <li><a href="/events">events</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         <p>
         Source code at <a href="https://github.com/tippenein/rsvp">https://github.com/tippenein/rsvp/</a>
         </p>
         </body>
         <html>
         |]
