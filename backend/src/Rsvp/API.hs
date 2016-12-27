{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for rsvp.
module Rsvp.API where

import           Protolude

import           Data.Int (Int64)
-- import Data.Aeson (ToJSON(..))
import Data.Aeson.Types (Options, fieldLabelModifier, defaultOptions)
import qualified Data.Text as T
-- import           Database.Persist.Types (Entity(..))
import qualified NeatInterpolation as NI
import qualified Prelude
import           Servant.API

import           Shared.Types
import           Rsvp.API.Internal (HTML)

type API = RsvpAPI :<|> Raw
type RsvpAPI =
  Get '[HTML] RootPage :<|>
  "users" :> Get '[JSON] UserResponse :<|>
  "rsvps" :> Get '[JSON] RsvpResponse :<|>
  GetEvent:<|>
  ListEvents :<|>
  CreateEvent

type GetEvent =
  "events" :> Capture "id" Int64 :> Get '[JSON] Event

type ListEvents =
  "events" :> QueryParam "name" Text :> Get '[JSON] EventResponse

type CreateEvent =
  "events"
  :> ReqBody '[JSON] Event
  :> Post '[JSON] EventCreateResponse

api :: Proxy API
api = Proxy

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

data RootPage =
  RootPage

instance MimeRender HTML RootPage where
  mimeRender _ _ =
    toS
      [NI.text|
        <!DOCTYPE html>
        <html>
          <head>
            <script language="javascript" src="rts.js"></script>
            <script language="javascript" src="lib.js"></script>
            <script language="javascript" src="out.js"></script>
          </head>
          <body>
            <ul>
            <li><a href="/users">users</a></li>
            <li><a href="/events">events</a></li>
            <li><a href="/rsvps">rsvps</a></li>
            <li><a href="/metrics"><code>/metrics</code></a></li>
            </ul>
          </body>
          <script language="javascript" src="runmain.js" defer></script>
        </html>
      |]
