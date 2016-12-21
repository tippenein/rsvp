{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for rsvp.
module Rsvp.API where

import Protolude

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Prelude
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
  :> Post '[JSON] EventCreateResponse

api :: Proxy API
api = Proxy

data User = User
  { _user_id :: Int
  , _user_first_name :: Text
  , _user_last_name :: Text
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

-- json -----------------------------------------------
instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = dropIdentifier }
instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = dropIdentifier }

instance FromJSON Rsvp where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = dropIdentifier }
instance ToJSON Rsvp where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = dropIdentifier }

instance FromJSON Event where
  parseJSON = genericParseJSON defaultOptions {
                fieldLabelModifier = dropIdentifier }
instance ToJSON Event where
  toJSON = genericToJSON defaultOptions {
             fieldLabelModifier = dropIdentifier }

instance FromJSON ContactInfo
instance ToJSON ContactInfo

dropIdentifier :: Prelude.String -> Prelude.String
dropIdentifier = T.unpack . T.intercalate "_" . drop 1 . T.splitOn "_" . T.pack

data RootPage =
  RootPage

instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
         <!doctype html>
         <html>
         <head><title>RSVP</title></head>
         <body>
         <h1>RSVP</h1>
         <ul>
         <li><a href="/users">users</a></li>
         <li><a href="/events">events</a></li>
         <li><a href="/rsvps">events</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         </body>
         <html>
         |]

-- serializers ------------------------------
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


-- instance ToJSON (Entity Event) where
--   toJSON = genericToJSON defaultOptions
  -- toJSON (Entity pid (Event )) = object
  --   [ "id" .= pid
  --   , "name" .= _event_name
  --   ]

type Status = Either Text Text

newtype EventCreateResponse =
  EventCreateResponse Status
  deriving (Eq, Show, Generic)

instance FromJSON EventCreateResponse where
  parseJSON (Object v) = EventCreateResponse <$> v .: "status"
  parseJSON x = typeMismatch "Events" x


instance ToJSON EventCreateResponse where
  toJSON (EventCreateResponse event) = object ["event" .= toJSON event]
