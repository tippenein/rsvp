{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

-- | Implementation of the rsvp API.
module Rsvp.Server.Handlers
  ( server
  ) where

import           Protolude hiding (Handler, get)

import           Control.Monad.Except (ExceptT(..))
import           Control.Monad.Log (logInfo)
import           Control.Monad.Reader (ReaderT, runReaderT)
import           Database.Persist
import           Database.Persist.Sql (toSqlKey)
import           Servant (ServantErr, Server, (:<|>)(..), (:~>)(..), enter)
import           Text.PrettyPrint.Leijen.Text (Doc, Pretty, text)

import qualified Rsvp.Server.Config as Config
import           Rsvp.API
import           Rsvp.Response
import           Rsvp.Server.Models
import qualified Rsvp.Server.Logging as Log

-- | rsvp API implementation.
server :: Config.Config -> Server API
server config = enter (toHandler config) handlers
  where
    handlers =
      pure RootPage :<|>
      users :<|>
      rsvps :<|>
      getEvent :<|>
      events :<|>
      createEvent
      -- files

-- | Our custom handler type.
type Handler msg = ReaderT Config.Config (ExceptT ServantErr (Log.LogM msg IO))


-- | Translate our custom monad into a Servant handler.
--
-- See http://haskell-servant.readthedocs.io/en/stable/tutorial/Server.html#using-another-monad-for-your-handlers
-- for the details.
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

-- convertApp :: Config.Config -> Config.App :~> ExceptT ServantErr IO
-- convertApp cfg = Nat (flip runReaderT cfg . Config.runApp)

users :: Handler Doc UserResponse
users = do
  logInfo (text "Example of logging")
  -- pure (UserResponse [User "Isaac" (Phone "1234445556"), User "Albert" (Email "whatever@whatever.com")])
  pure (UserResponse [User "Isaac" "1234445556", User "Albert" "whatever@whatever.com"])

getEvent :: Int64 -> Handler Doc Event
getEvent id = do
  logInfo (text $ "getting event " <> show id)
  pure (Event (toSqlKey id) "event brawl" "1112223434")
  -- record <- runDb $ get (toSqlKey id)
  -- case record of
  --   Nothing -> throwError err404
  --   Just a -> entityVal a

events :: Maybe Text -> Handler Doc EventResponse
events _ = do
  es <- runDb $ selectList [] []
  pure $ EventResponse $ fmap entityVal es
  -- case mname of
  -- Nothing -> do
  --   logInfo (text "showing all events")
  --   -- pure (EventResponse [Event 1 (Phone "1112223434") "event brawl", Event 2 (Email "derp@derp.com") "whatever event"])
  --   pure (EventResponse [Event (toSqlKey 1) "1112223434" "event brawl", Event (toSqlKey 2) "derp@derp.com" "whatever event"])
  -- Just name -> do
  --   logInfo (text $ "showing " <> show name)
  --   pure (EventResponse [Event (toSqlKey 1) "1112223434" "event brawl"])

rsvps :: Handler Doc RsvpResponse
rsvps = do
  logInfo (text "showing all Rsvps")
  pure (RsvpResponse [])

createEvent :: Event -> Handler Doc EventCreateResponse
createEvent event = do
  putText $ show event
  -- event_id <- runDb $ insert event
  logInfo (text "created new event ")
  pure (EventCreateResponse (Right "success"))

-- files = do
--   logInfo (text "showing assets")
--   serveDirectory "assets"
