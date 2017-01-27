{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import           Servant (serveDirectory, err404, err400, ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import qualified Servant.Server.Auth.Token as Auth
import           Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

import           Rsvp.API
import qualified Rsvp.Server.Config as Config
import qualified Rsvp.Server.Logging as Log
import           Rsvp.Server.Models
import qualified Shared.Models as Model
import           Shared.Types

files :: Application
files = serveDirectory "public"

rsvpServer :: Config.Config -> Server RsvpAPI
rsvpServer config = enter (toHandler config) handlers
  where
    handlers =
      users :<|>
      rsvps :<|>
      createRsvp :<|>
      getEvent :<|>
      getEventImage :<|>
      events :<|>
      createEvent

-- | rsvp API implementation.
server :: Config.Config -> Server API
server config = enter (toHandler config) handlers
  :<|> Auth.authServer (Config.authConfig config)
  :<|> pure RootPage
  :<|> files
  where
    handlers =
      users :<|>
      rsvps :<|>
      createRsvp :<|>
      getEvent :<|>
      getEventImage :<|>
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

users :: PaginationParams (Handler Doc (PaginatedResponse Model.User))
users page per_page = do
  us <- runDb $ selectList [] (paginationParams page per_page)
  pure (PaginatedResponse 1 1 us)

getEvent :: DbKey -> Handler Doc Model.Event
getEvent = selectById

getEventImage :: DbKey -> Handler Doc ByteString
getEventImage id = do
  record <- runDb $ get (toSqlKey id)
  case record of
    Nothing -> throwError err404
    Just a -> pure $ fromMaybe "" $ Model.eventImage a

createRsvp :: Model.Rsvp -> Handler Doc (CreateResponse Model.Rsvp)
createRsvp rsvp = do
  e <- runDb $ get $ Model.rsvpEvent_id rsvp
  case e of
    Just _ -> createResource rsvp
    Nothing -> throwError err400

-- createEvent :: Model.Event -> Handler Doc (CreateResponse Model.Event)
-- createEvent = createResource
createEvent :: Model.Event -> Handler Doc (CreateResponse Model.Event)
createEvent e = do
  u <- runDb $ get $ Model.eventCreator_id e
  case u of
    Just _ -> createResource e
    Nothing -> throwError err400

events :: Maybe Text -> PaginationParams (Handler Doc (PaginatedResponse Model.Event))
events mname page per_page =
  case mname of
    Nothing -> do
      es <- runDb $ selectList [] (paginationParams page per_page)
      pure (PaginatedResponse 1 1 es)
    Just name -> do
      logInfo (text $ "showing matches to: " <> show name)
      es <- runDb $ select $ from $ \events' -> do
        where_ (events' ^. Model.EventName `like` (%) ++. val name ++. (%))
        orderBy [asc (events' ^. Model.EventTimeStart)]
        pure events'
      pure (PaginatedResponse 1 1 es)

rsvps :: PaginationParams (Handler Doc (PaginatedResponse Model.Rsvp))
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

createResource :: (ToBackendKey SqlBackend a, Show a) => a -> Handler Doc (CreateResponse a)
createResource resource = do
  putText $ show resource
  _resource_id <- runDb $ insert resource
  logInfo (text $ "created new resource " <> show resource)
  let rsp = CreateResponse { _message = Success "successfully created resource"
                           , _db_id = fromSqlKey _resource_id
                           , _posted_content = resource }
  pure rsp
