{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           Control.Lens hiding (view)
import           Data.Aeson (ToJSON(..), Value(..), encode)
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Database.Persist.Sql (toSqlKey)
import           Reflex
import           Reflex.Dom

import           Common
import qualified Component
import           Request
import           Shared.Types hiding (Event)
import qualified Shared.Types as Shared
import qualified Widget

import           Protolude hiding (div, (&), ByteString)
import           Prelude ()

data Action
  = InitialLoad
  | Query Text
  | SelectEvent Int
  | NewEvent
  | NewUser User
  | ToggleEventForm
  | EventsPayload EventResponse
  | CreateEventPayload EventCreateResponse

data Model
  = Model
  { _events :: Map Int RsvpEvent
  , _query :: Text
  , _status :: Maybe Status
  , _show_event_form :: Bool
  , _selected :: Maybe Int
  } deriving (Show)

initialModel :: Model
initialModel
  = Model
  { _events = Map.empty
  , _query = ""
  , _status = Nothing
  , _show_event_form = False
  , _selected = Nothing
  }

update :: Action -> Model -> Model
update InitialLoad m = m
update (SelectEvent e) m = m { _selected = pure e }
update (Query s) m = m { _query = s  }
update (EventsPayload (EventResponse rsp)) m = m { _events = Map.fromList $ zip [(1::Int)..] rsp }
update NewEvent m = m { _status = pure $ Info "creating new event" }
update (NewUser _) m = m
update (ToggleEventForm) m = m { _show_event_form = not $ _show_event_form m }
update (CreateEventPayload (EventCreateResponse res)) m = m { _status = pure res }

banner :: MonadWidget t m => m ()
banner = elAttr "h2" ("style" =: "text-align: center") $ text "rsvp"

-- footer :: MonadWidget t m => Dynamic t Model -> m ()
-- footer _model = elClass "footer" "footer" $ do
--   text "bye from reflex"
--   pure ()

view :: MonadWidget t m
     => Dynamic t Model
     -> m (Event t Action)
view model = div "container" $ do
  div "errors row" $ do
    -- let status = fmap _status model
    -- Component.showStatus status
    display $ fmap _status model
    pure ()

  newEventClick <- elClass "div" "pull-right" $ do
    let v = fmap (`monoidGuard` Component.noDisplay) $ fmap _show_event_form model
    let val = zipDynWith Map.union v (constDyn $ "value" =: "new event")
    btnDynAttr (constDyn ("class" =: "btn btn-primary") <> val)


  (eventCreateResponse, submitEvent, cancelEvent) <- Component.eventForm (fmap _show_event_form model)

  (eventsResponse, requestEvents) <- searchForm

  let selectedEvent = fmap _selected model
  eventSelected <- Component.eventListing (fmap _events model) selectedEvent

  pure $ leftmost
      [ EventsPayload <$> eventsResponse
      , SelectEvent <$> eventSelected
      , NewEvent <$ submitEvent
      , ToggleEventForm <$ leftmost [newEventClick, cancelEvent]
      , CreateEventPayload <$> eventCreateResponse
      , requestEvents
      ]

searchForm :: MonadWidget t m => m (Event t EventResponse, Event t Action)
searchForm = div "row" $ do
  postBuild <- getPostBuild
  q <- Widget.searchInput

  let query = _textInput_value q
  let requestEvents = leftmost [ Query <$> updated query
                                , InitialLoad <$ postBuild
                                ]
  rsp :: Event t XhrResponse <- performRequestAsync $ mkGET EventsRoute <$> requestEvents
  let eventsResponse :: Event t EventResponse = fmapMaybe decodeXhrResponse rsp
  pure (eventsResponse, requestEvents)

bodyElement :: MonadWidget t m => m ()
bodyElement = do
  banner
  mainContainer $ do
    rec changes <- view model
        model <- foldDyn update initialModel changes

    pure ()

mainContainer :: MonadWidget t m => m () -> m ()
mainContainer body = do
  div "container"
    body
  pure ()

div :: MonadWidget t m => Text -> m a -> m a
div = elClass "div"


mkGET :: Route -> Action -> XhrRequest ()
mkGET u InitialLoad = XhrRequest "GET" (defaultUrl u) def
mkGET u (Query q) = XhrRequest "GET" uri def
  where
    uri = defaultUrl u <> "?name=" <> q
mkGET _ _ = Protolude.error "invalid req action"
