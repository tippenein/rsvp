{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for rsvp.
module Rsvp.API where

import           Protolude

import           Data.Int (Int64)
import qualified NeatInterpolation as NI
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
