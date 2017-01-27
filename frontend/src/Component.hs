{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Component represents business-logic specific dom elements
module Component where

-- import           Data.ByteString
-- import qualified Data.ByteString.Base64 as B64
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Time.Clock (UTCTime)
import           Data.Time (formatTime, defaultTimeLocale)
import           Database.Persist.Sql (toSqlKey)
import           Reflex.Dom
------------------------------------
import           Common
import qualified Shared.Types as Shared
import           Shared.Types
import qualified Widget
import qualified Shared.Models as Model
import           Request

import           Protolude hiding (div, (&), log)
import           Prelude ()
------------------------------------

eventForm :: MonadWidget t m => Dynamic t Bool -> m (Event t EventCreateResponse, Dynamic t Model.Event, Event t (), Event t ())
eventForm visible = do
  let attrs' = fmap (`Common.monoidGuard` blockDisplay ) visible
  let attrs = zipDynWith Map.union attrs' (constDyn noDisplay)
  elDynAttr "div" attrs $ do
    (eventSubmitData, submitEvent, cancelEvent) <- mkEventForm
    let postData = Request.toPostWithEncode (defaultUrl EventsRoute) <$> eventSubmitData

    createRsp :: Event t XhrResponse <- performRequestAsync $ attachPromptlyDynWith const postData submitEvent
    let eventCreateResponse :: Event t EventCreateResponse = fmapMaybe decodeXhrResponse createRsp
    pure (eventCreateResponse, eventSubmitData, submitEvent, cancelEvent)

mkEventForm :: MonadWidget t m => m (Dynamic t Model.Event, Event t (), Event t ())
mkEventForm = do
  let attrs = mconcat ["class" =: "form event-form"
                      , "id" =: "event-form"
                      , "action" =: "/events"
                      , "method" =: "post"
                      ]
  (submitEvent, cancelEvent, nameDyn, contactDyn, fileDyn, timeStart, timeEnd) <- elAttr "form" attrs $ do
    nameDyn <- Widget.formGroup "text" "eventName" "Event Name"
    contactDyn <- Widget.formGroup "text" "contactInfo" "Contact Info"
    timeStart <- Widget.datePicker
    timeEnd <- Widget.datePicker
    fileEvent <- div "row" $ Widget.bootstrapFileInput "Event Logo"
    fileDyn <- holdDyn Nothing (Just <$> fileEvent)

    cancelButton <- btn "cancel"
    submitButton <- btnClass "submit" "btn btn-primary"
    pure (submitButton, cancelButton, nameDyn, contactDyn, fileDyn, timeStart, timeEnd)

  let e = Model.Event (toSqlKey 1) <$> nameDyn <*> contactDyn <*> timeStart <*> timeEnd <*> fileDyn
  pure (e, submitEvent, cancelEvent)

eventListing :: MonadWidget t m
  => Dynamic t (Map DbKey Model.Event)
  -> Dynamic t (Maybe DbKey)
  -> m (Event t DbKey)
eventListing eventMap selectedEvent = elClass "div" "row event-listing" $
  Widget.selectableListWithKey selectedEvent eventMap $ \sel db_key rsvp_event -> do
    e <- eventEl sel db_key rsvp_event
    pure $ db_key <$ domEvent Click e

eventEl :: (MonadWidget t m)
   => Dynamic t Bool
   -> DbKey
   -> Dynamic t Model.Event
   -> m (El t)
eventEl sel db_key rsvp_event = do
  let commonAttrs = constDyn $ "class" =: "panel panel-default event-wrap"
  let attrs = fmap (`Common.monoidGuard` selectedStyle) sel
  (e,_) <- elDynAttr' "div" (zipDynWith classMerge attrs commonAttrs) $ do
    elClass "div" "panel-heading" $ dynText $ fmap Model.eventName rsvp_event
    elClass "div" "panel-body" $ do
      dynText $ fmap Model.eventContact rsvp_event
      br
      text "start time"
      dynText $ fmap (timeF . Model.eventTimeStart) rsvp_event
      br
      text "end time"
      dynText $ fmap (timeF . Model.eventTimeEnd) rsvp_event
      br
      elClass "div" "img-wrapper" $ dyn $ fmap (imgBinEl db_key . Model.eventImage) rsvp_event
  pure e

timeF :: Maybe UTCTime -> Text
timeF t = case t of
  Just t' -> T.pack $ formatTime defaultTimeLocale "%M %t" t'
  Nothing -> "TBD"

imgBinEl :: MonadWidget t m => DbKey -> (Maybe ByteString) -> m ()
imgBinEl db_key mimage =
  let attrs = eventImageAttrs $ def { ei_img_src = getSrc mimage }
  in elAttr "img" attrs blank
    where
      getSrc :: Maybe ByteString -> Maybe Text
      getSrc Nothing = Nothing
      getSrc (Just _) = Just $ "/events/" <> show db_key <> "/image"

flashStatus :: MonadWidget t m => Dynamic t (Maybe Status) -> m (Event t ())
flashStatus status = div "errors row" $ do
  cs <- widgetHoldHelper showStatus Nothing $ updated status
  pure $ switchPromptlyDyn cs


showStatus :: MonadWidget t m => Maybe Status -> m (Event t ())
showStatus (Just status) = flash status
showStatus Nothing = pure never

dismissableAlert :: (DomBuilder t m) => Status -> m (Event t ()) -> m (Event t ())
dismissableAlert st body = do
  let cls = T.intercalate " " ["alert", "alert-" <> clsFor st, "alert-dismissible"]
  let attrs = mconcat [ "class" =: cls, "role" =: "alert"]
  elAttr "div" attrs body
  where
    clsFor (Shared.Success _) = "success"
    clsFor (Shared.Warning _) = "warning"
    clsFor (Shared.Info    _) = "info"
    clsFor (Shared.Error   _) = "error"

adminControl :: MonadWidget t m => Dynamic t Bool -> m (Event t ())
adminControl showEventForm = elClass "div" "pull-right" $ do
  let v = fmap (`monoidGuard` Component.noDisplay) showEventForm
  let val = zipDynWith Map.union v (constDyn $ "value" =: "new event")
  btnDynAttr (constDyn ("class" =: "btn btn-primary") <> val)

flash :: MonadWidget t m => Status -> m (Event t ())
flash status =
  dismissableAlert status $ do
    let attr = mconcat [ "type" =: "button"
                       , "class" =: "close"
                       , "data-dismiss" =: "alert"
                       , "aria-label" =: "Close"
                       ]
    c <- btnAttr "×" attr
    text $ show status
    pure c

data EventImageConfig = EventImageConfig
  { ei_height :: Int
  , ei_width :: Int
  , ei_img_src :: Maybe Text
  }

instance Default EventImageConfig where
  def = EventImageConfig
    { ei_width = 300
    , ei_height = 300
    , ei_img_src = Nothing }


eventImageAttrs :: EventImageConfig -> ElAttrs
eventImageAttrs conf =
  let w = ei_width conf
      h = ei_height conf
      src = fromMaybe (Widget.placeholderImageLocation w h) (ei_img_src conf)
  in
    mconcat [ "src" =: src
            , "width" =: show w
            , "height" =: show h ]

selectedStyle :: Map Text Text
selectedStyle = "class" =: "selected"

noDisplay :: Map Text Text
noDisplay = "style" =: "display: none"

blockDisplay :: Map Text Text
blockDisplay = "style" =: "display: block"

br :: MonadWidget t m => m ()
br = el "br" blank
