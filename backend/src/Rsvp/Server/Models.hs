{-# LANGUAGE FlexibleContexts #-}
module Rsvp.Server.Models where

import           Control.Monad.Reader
import           Database.Persist.Sql

import Protolude

import Rsvp.Server.Config
import Shared.Types

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
