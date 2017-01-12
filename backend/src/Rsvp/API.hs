{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

-- | API definition for rsvp.
module Rsvp.API where

import           Protolude

import qualified NeatInterpolation as NI
import           Servant.API
import qualified Servant.API.Auth.Token as Auth

import           Shared.Types
import           Rsvp.API.Internal (HTML, SelectById)

type Paginate c cts a =
  QueryParam "page" Int :>
  QueryParam "per_page" Int :>
  c cts (PaginatedResponse a)

type API = RsvpAPI :<|> Auth.AuthAPI :<|> Raw

type RsvpAPI =
  Get '[HTML] RootPage :<|>
  "users" :> Paginate Get '[JSON] User :<|>
  "rsvps" :> Paginate Get '[JSON] Rsvp :<|>
  GetEvent :<|>
  ListEvents :<|>
  CreateEvent

type GetEvent =
  "events" :> SelectById "id" Event

type ListEvents =
  "events" :> QueryParam "name" Text :> Paginate Get '[JSON] Event

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
