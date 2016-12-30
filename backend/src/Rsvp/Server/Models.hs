{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
module Rsvp.Server.Models where

import           Control.Monad.Reader
import qualified Database.Persist.Sql as Sql
import           Rsvp.Server.Config
import           Shared.Types (migrateAll)

import           Protolude

toKey :: forall record . Sql.ToBackendKey Sql.SqlBackend record => Int64 -> Sql.Key record
toKey = Sql.toSqlKey

doMigrations :: Sql.SqlPersistT IO ()
doMigrations = Sql.runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => Sql.SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ Sql.runSqlPool query pool
