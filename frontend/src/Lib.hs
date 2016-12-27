{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Int (Int64)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Monoid ((<>))
import           Data.Text (Text)
import           GHC.Generics
import           Prelude hiding (div)
import           Reflex
import           Reflex.Dom

import qualified Widget
import qualified Common
import Shared.Types hiding (Event)

data Action
  = InitialLoad
  | Query Text
  | SelectEvent Int
  | EventsResponse [RsvpEvent]

data Model
  = Model
  { _events :: Map Int RsvpEvent
  , _query :: Text
  , _selected :: Maybe Int
  } deriving (Show)

initialModel :: Model
initialModel
  = Model
  { _events = Map.empty
  , _query = ""
  , _selected = Nothing
  }

update :: Action -> Model -> Model
update InitialLoad m = m
update (SelectEvent e) m = m { _selected = pure e }
update (Query s) m = m { _query = s }
update (EventsResponse rsp) m = m { _events = Map.fromList $ zip [(1::Int)..] rsp}

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
  (eventsResponse, requestEvents) <- div "row" $ do
    postBuild <- getPostBuild
    q <- Widget.searchInput

    let query = _textInput_value q
    let requestEvents = leftmost [ Query <$> updated query
                                , InitialLoad <$ postBuild
                                ]
    rsp :: Event t XhrResponse <- performRequestAsync $ mkReqTo "events" <$> requestEvents
    let eventsResponse :: Event t [RsvpEvent] = fmapMaybe decodeXhrResponse rsp
    pure (eventsResponse, requestEvents)

  let eventMap = fmap _events model
  let selectedEvent = fmap _selected model
  eventSelected <- divClass "row" $ do
    bs' <- divClass "event-list"$ do
      bs <- el "ul" $ Widget.selectableList selectedEvent eventMap $ \sel p -> do
        domEvent Click <$> eventEl sel p
      pure bs
    display $ zipDynWith maybeLookup selectedEvent eventMap
    pure bs'

  pure $ leftmost
      [ EventsResponse <$> eventsResponse
      , SelectEvent <$> eventSelected
      , requestEvents
      ]

maybeLookup :: Maybe Int -> Map.Map Int a -> Maybe a
maybeLookup midx m = case midx of
  Nothing -> Nothing
  Just idx -> Map.lookup idx m
eventEl :: (MonadWidget t m)
   => Dynamic t Bool
   -> Dynamic t RsvpEvent
   -> m(El t)
eventEl sel b = do
  let commonAttrs = constDyn $ "class" =: "event-wrap"
  let attrs = fmap (\s -> Common.monoidGuard s $ selectedStyle ) sel
  (e,_) <- elDynAttr' "li" (attrs <> commonAttrs) $ do
    dynText $ fmap eventName b
    text " - "
    dynText $ fmap eventContact b
  pure e

selectedStyle :: Map Text Text
selectedStyle = "style" =: "border: solid 1px #4748f9"

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

defaultUrl :: Text -> Text
defaultUrl t = "http://localhost:8081/" <> t

mkReqTo :: Text -> Action -> XhrRequest ()
mkReqTo u InitialLoad = XhrRequest "GET" (defaultUrl u) def
mkReqTo u (Query q) = XhrRequest "GET" uri def
  where
    uri = defaultUrl u <> "?q=" <> q
mkReqTo _ _ = error "invalid req action"
