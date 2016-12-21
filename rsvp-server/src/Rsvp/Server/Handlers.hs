{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the rsvp API.
module Rsvp.Server.Handlers
  ( server
  ) where

import Protolude hiding (Handler)

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity, logInfo)
import Servant (ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

import Rsvp.API
import qualified Rsvp.Server.Logging as Log

-- | rsvp API implementation.
server :: Severity -> Server API
server logLevel = enter (toHandler logLevel) handlers
  where
    handlers =
      pure RootPage :<|>
      users :<|>
      rsvps :<|>
      events :<|>
      createEvent

-- | Our custom handler type.
type Handler msg = ExceptT ServantErr (Log.LogM msg IO)

-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
toHandler
  :: Pretty msg
  => Severity -> (Handler msg :~> ExceptT ServantErr IO)
toHandler logLevel = Nat toHandler'
  where
    toHandler'
      :: Pretty msg
      => Handler msg a -> ExceptT ServantErr IO a
    toHandler' = ExceptT . Log.withLogging logLevel . runExceptT

users :: Handler Doc UserResponse
users = do
  logInfo (text "Example of logging")
  pure (UserResponse [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"])

events :: Handler Doc EventResponse
events = do
  logInfo (text "showing all events")
  pure (EventResponse [Event 1 (Phone "1112223434") "event brawl", Event 2 (Email "derp@derp.com") "whatever event"])


rsvps :: Handler Doc RsvpResponse
rsvps = do
  logInfo (text "showing all Rsvps")
  pure (RsvpResponse [])

createEvent :: Event -> Handler Doc EventResponse
createEvent event = do
  putText $ show event
  logInfo (text "creating new event ")
  pure (EventResponse [])
