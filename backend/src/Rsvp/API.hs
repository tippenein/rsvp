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
  "users" :> Get '[JSON] (ListResponse User) :<|>
  "rsvps" :> Get '[JSON] (ListResponse Rsvp) :<|>
  GetEvent:<|>
  ListEvents :<|>
  CreateEvent

type GetEvent =
  "events" :> Capture "id" Int64 :> Get '[JSON] Event

type ListEvents =
  "events" :> QueryParam "name" Text :> Get '[JSON] (ListResponse Event)

type CreateEvent =
  "events"
  :> ReqBody '[JSON] Event
  :> Post '[JSON] EventCreateResponse


rsvpApi :: Proxy RsvpAPI
rsvpApi = Proxy

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
            <link rel="stylesheet" type="text/css" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css">
            <link rel="stylesheet" type="text/css" href="http://fonts.googleapis.com/css?family=Lato">
            <link rel="stylesheet" type="text/css" href="https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/3.3.7/css/bootstrap.min.css">
            <link rel="stylesheet" type="text/css" href="css/style.css">
          </head>
          <body>
          </body>
          <script language="javascript" src="runmain.js" defer></script>
        </html>
      |]
