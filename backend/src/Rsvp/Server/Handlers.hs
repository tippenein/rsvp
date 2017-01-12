{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

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
import           Database.Persist.Sql (toSqlKey, selectList, SelectOpt(..))
import           Network.Wai (Application)
import           Servant (serveDirectory, err404, ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import qualified Servant.Server.Auth.Token as Auth
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
server config = enter (toHandler config) handlers
  :<|> Auth.authServer (Config.authConfig config)
  :<|> files
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

type PaginationParams next = Maybe Int -> Maybe Int -> next

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

users :: PaginationParams (Handler Doc (PaginatedResponse User))
users page per_page = do
  us <- runDb $ selectList [] (paginationParams page per_page)
  pure (PaginatedResponse 1 1 us)

getEvent :: DbKey -> Handler Doc Event
getEvent = selectById
  -- record <- runDb $ get (toSqlKey id)
  -- case record of
  --   Nothing -> throwError err404
  --   Just a -> pure a

createEvent :: Event -> Handler Doc EventCreateResponse
createEvent event = do
  putText $ show event
  _event_id <- runDb $ insert event
  logInfo (text $ "created new event " <> show event)
  let rsp = CreateResponse { _message = Success "successfully created event"
                           , _db_id = fromSqlKey _event_id
                           , _posted_content = event }
  pure $ EventCreateResponse rsp

events :: Maybe Text -> PaginationParams (Handler Doc (PaginatedResponse Event))
events mname page per_page =
  case mname of
    Nothing -> do
      es <- runDb $ selectList [] (paginationParams page per_page)
      pure (PaginatedResponse 1 1 es)
    Just name -> do
      logInfo (text $ "showing matches to: " <> show name)
      es <- runDb $ select $ from $ \events' -> do
        where_ (events' ^. EventName `like` (%) ++. val name ++. (%))
        pure events'
      pure (PaginatedResponse 1 1 es)

rsvps :: PaginationParams (Handler Doc (PaginatedResponse Rsvp))
rsvps page per_page = do
  rs <- runDb $ selectList [] (paginationParams page per_page)
  pure (PaginatedResponse 1 1 rs)

--- helpers
paginationParams :: forall record. Maybe Int -> Maybe Int -> [SelectOpt record]
paginationParams page per_page =
  let pp = fromMaybe 6 per_page
      off = pp * fromMaybe 0 page
  in [OffsetBy off, LimitTo pp]

selectById :: ( PersistEntityBackend record ~ SqlBackend, ToBackendKey SqlBackend record)
  => DbKey
  -> Handler Doc record
selectById ident = do
  result <- runDb $ get (toSqlKey ident)
  case result of
    Nothing -> throwError err404
    Just r  -> pure r
