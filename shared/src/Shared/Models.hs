{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}

module Shared.Models where

import           Data.Time.Clock (UTCTime)
import           Database.Persist.TH ( mkMigrate, mkPersist, persistLowerCase
                                     , share, sqlSettings
                                     )
import           Protolude

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
  name Text
  email Text
  deriving Eq Show

Rsvp sql=rsvps
  event_id EventId
  name Text
  contact Text
  deriving Eq Show

EventRsvps sql=event_rsvps
  event_id EventId
  rsvp_id RsvpId
  deriving Eq Show

Event sql=events
  creator_id UserId
  name Text
  contact Text
  timeStart UTCTime Maybe
  timeEnd UTCTime Maybe
  image ByteString Maybe
  deriving Eq Show
|]
