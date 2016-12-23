{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for rsvp.
module Rsvp.API where

import           Protolude

import           Data.Int (Int64)
-- import qualified Data.Text as T
-- import           Database.Persist.Types (Entity(..))
import qualified NeatInterpolation as NI
-- import qualified Prelude
import           Servant.API

import           Rsvp.Server.Models
import           Rsvp.Response
import           Rsvp.API.Internal (HTML)

type API =
  Get '[HTML] RootPage :<|>
  "users" :> Get '[JSON] UserResponse :<|>
  "rsvps" :> Get '[JSON] RsvpResponse :<|>
  "events" :> Capture "id" Int64 :> Get '[JSON] Event :<|>
  "events" :> QueryParam "name" Text :> Get '[JSON] EventResponse :<|>
  CreateEvent -- :<|>
  -- Raw

type CreateEvent =
  "events"
  :> ReqBody '[JSON] Event
  :> Post '[JSON] EventCreateResponse

api :: Proxy API
api = Proxy

-- data User = User
--   { _user_name :: Text
--   , _user_contact :: ContactInfo
--   } deriving (Eq, Show, Generic)

-- data ContactInfo
--   = Phone Text
--   | Email Text
--   deriving (Eq, Show, Generic)

-- data Rsvp
--   = Rsvp
--   { _rsvp_event_id :: Int64
--   , _rsvp_name :: Text
--   , _rsvp_contact :: ContactInfo
--   } deriving (Eq, Show, Generic)

-- data Event = Event
--   { _event_creator_id :: Int64 -- User id
--   , _event_creator_contact :: ContactInfo
--   , _event_name:: Text
--   } deriving (Eq, Show, Generic)

-- -- json -----------------------------------------------
-- instance FromJSON User where
--   parseJSON = genericParseJSON defaultOptions {
--                 fieldLabelModifier = dropIdentifier }
-- instance ToJSON User where
--   toJSON = genericToJSON defaultOptions {
--              fieldLabelModifier = dropIdentifier }

-- instance FromJSON Rsvp where
--   parseJSON = genericParseJSON defaultOptions {
--                 fieldLabelModifier = dropIdentifier }
-- instance ToJSON Rsvp where
--   toJSON = genericToJSON defaultOptions {
--              fieldLabelModifier = dropIdentifier }

-- instance FromJSON Event where
--   parseJSON = genericParseJSON defaultOptions {
--                 fieldLabelModifier = dropIdentifier }
-- instance ToJSON Event where
--   toJSON = genericToJSON aesonDef

-- aesonDef :: Options
-- aesonDef = defaultOptions { fieldLabelModifier = dropIdentifier }

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

-- dropIdentifier :: Prelude.String -> Prelude.String
-- dropIdentifier = T.unpack . T.intercalate "_" . drop 2 . T.splitOn "_" . T.pack

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
         <li><a href="/rsvps">rsvps</a></li>
         <li><a href="/metrics"><code>/metrics</code></a></li>
         </ul>
         </body>
         <html>
         |]
