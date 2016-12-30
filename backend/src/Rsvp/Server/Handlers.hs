{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the rsvp API.
module Rsvp.Server.Handlers
  ( server
  , rsvpServer
  ) where

import           Protolude hiding (get, (%), from)

import           Control.Monad.Except (ExceptT(..))
import           Control.Monad.Log (logInfo)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Database.Esqueleto
import           Database.Persist.Sql (toSqlKey, selectList)
import           Network.Wai (Application)
import           Servant (serveDirectory, err404, ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import           Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

import           Rsvp.API
import           Shared.Types
import qualified Rsvp.Server.Config as Config
import qualified Rsvp.Server.Logging as Log
import           Rsvp.Server.Models

files :: Application
files = serveDirectory "public"

rsvpServer :: Config.Config -> Server RsvpAPI
rsvpServer config = enter (toHandler config) handlers
  where
    handlers =
      pure RootPage :<|>
      users :<|>
      rsvps :<|>
      getEvent :<|>
      events :<|>
      createEvent

-- | rsvp API implementation.
server :: Config.Config -> Server API
server config = enter (toHandler config) handlers :<|> files
  where
    handlers =
      pure RootPage :<|>
      users :<|>
      rsvps :<|>
      getEvent :<|>
      events :<|>
      createEvent

-- | Our custom handler type.
type Handler msg = ReaderT Config.Config (ExceptT ServantErr (Log.LogM msg IO))


-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
toHandler
  :: Pretty msg
  => Config.Config -> Handler msg :~> ExceptT ServantErr IO
toHandler config = Nat (convertToHandler config)

convertToHandler
  :: Pretty msg
  => Config.Config
  -> Handler msg a
  -> ExceptT ServantErr IO a
convertToHandler cfg = ExceptT . Log.withLogging (Config.logLevel cfg) . runExceptT . flip runReaderT cfg

users :: Handler Doc UserResponse
users = do
  us <- runDb $ selectList [] []
  pure (UserResponse $ fmap entityVal us)

getEvent :: Int64 -> Handler Doc Event
getEvent id = do
  record <- runDb $ get (toSqlKey id)
  case record of
    Nothing -> throwError err404
    Just a -> pure a

createEvent :: Event -> Handler Doc EventCreateResponse
createEvent event = do
  putText $ show event
  _event_id <- runDb $ insert event
  logInfo (text $ "created new event " <> show event)
  pure (EventCreateResponse (Success "successfully created event"))

events :: Maybe Text -> Handler Doc EventResponse
events mname =
  case mname of
    Nothing -> do
      es <- runDb $ selectList [] []
      pure (EventResponse $ fmap entityVal es)
    Just name -> do
      logInfo (text $ "showing matches to: " <> show name)
      es <- runDb $ select $ from $ \events' -> do
        where_ (events' ^. EventName `like` (%) ++. val name ++. (%))
        pure events'
      pure (EventResponse $ fmap entityVal es)

rsvps :: Handler Doc RsvpResponse
rsvps = do
  es <- runDb $ selectList [] []
  pure (RsvpResponse $ fmap entityVal es)
