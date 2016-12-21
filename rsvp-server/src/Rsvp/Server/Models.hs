{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Rsvp.Server.Models where

import           Control.Monad.Reader
-- import           Data.Aeson           (FromJSON, ToJSON)
import           Database.Persist.Sql
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
-- import           GHC.Generics         (Generic)

import Protolude

import           Rsvp.API
import           Rsvp.Server.Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
UserDb sql=users
    name Text
    email Text
    deriving Eq Show

RsvpDb sql=rsvps
    event_id EventDbId
    name Text
    contact Text
    deriving Eq Show

EventDb sql=events
    name Text
    creator_id UserDbId
    contact Text
    deriving Eq Show
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks getPool
  liftIO $ runSqlPool query pool
