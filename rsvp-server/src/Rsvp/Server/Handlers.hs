{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the rsvp API.
module Rsvp.Server.Handlers
  ( server
  ) where

-- XXX: jml doesn't like the name "Handlers" for this, and isn't sure that it
-- should be in a submodule of Project.Server. Perhaps the code in
-- Project.Server (which is command-line processing, setting up logs &
-- monitoring, starting the HTTP server) should be in a different module.
import Protolude hiding (Handler)

import Control.Monad.Except (ExceptT(..))
import Control.Monad.Log (Severity, logInfo)
import Servant (ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

import Rsvp.API (API, RootPage(..), User(..), Users(..))
import qualified Rsvp.Server.Logging as Log

-- | rsvp API implementation.
server :: Severity -> Server API
server logLevel = enter (toHandler logLevel) handlers
  where
    handlers = pure RootPage :<|> users

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

-- | Example endpoint.
users :: Handler Doc Users
users = do
  logInfo (text "Example of logging")
  pure (Users [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"])
