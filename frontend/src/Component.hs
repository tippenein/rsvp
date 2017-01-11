{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Component represents business-logic specific dom elements
module Component where

-- import           Data.ByteString
-- import qualified Data.ByteString.Base64 as B64
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import           Database.Persist.Sql (toSqlKey)
import           Reflex.Dom
------------------------------------
import           Common
import qualified Shared.Types as Shared
import qualified Widget
import           Shared.Types hiding (Event)
import           Request

import           Protolude hiding (div, (&), ByteString, log)
import           Prelude ()
------------------------------------

eventForm :: MonadWidget t m => Dynamic t Bool -> m (Event t EventCreateResponse, Dynamic t RsvpEvent, Event t (), Event t ())
eventForm visible = do
  let attrs' = fmap (`Common.monoidGuard` blockDisplay ) visible
  let attrs = zipDynWith Map.union attrs' (constDyn noDisplay)
  elDynAttr "div" attrs $ do
    (eventSubmitData, submitEvent, cancelEvent) <- mkEventForm
    let postData = Request.toPostWithEncode (defaultUrl EventsRoute) <$> eventSubmitData

    createRsp :: Event t XhrResponse <- performRequestAsync $ attachPromptlyDynWith const postData submitEvent
    let eventCreateResponse :: Event t EventCreateResponse = fmapMaybe decodeXhrResponse createRsp
    pure (eventCreateResponse, eventSubmitData, submitEvent, cancelEvent)

mkEventForm :: MonadWidget t m => m (Dynamic t RsvpEvent, Event t (), Event t ())
mkEventForm = do
  let attrs = mconcat ["class" =: "form event-form"
                      , "id" =: "event-form"
                      , "action" =: "/events"
                      , "method" =: "post"
                      ]
  (submitEvent, cancelEvent, nameDyn, contactDyn, fileDyn) <- elAttr "form" attrs $ do
    nameDyn <- Widget.formGroup "text" "eventName" "Event Name"
    contactDyn <- Widget.formGroup "text" "contactInfo" "Contact Info"
    fileEvent <- Widget.bootstrapFileInput "Event Logo"
    fileDyn <- holdDyn Nothing (Just <$> fileEvent)

    cancelButton <- btn "cancel"
    submitButton <- btnClass "submit" "btn btn-primary"
    pure (submitButton, cancelButton, nameDyn, contactDyn, fileDyn)

  let e = Shared.Event (toSqlKey 1) <$> nameDyn <*> contactDyn <*> fileDyn
  pure (e, submitEvent, cancelEvent)

eventListing :: MonadWidget t m => Dynamic t (Map DbKey RsvpEvent) -> Dynamic t (Maybe DbKey) -> m (Event t DbKey)
eventListing eventMap selectedEvent = do
  eventSelected <- divClass "row" $
    elClass "div" "event-listing" $ Widget.selectableList selectedEvent eventMap $ \sel rsvp_event ->
      domEvent Click <$> eventEl sel rsvp_event
  pure eventSelected

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

eventEl :: (MonadWidget t m)
   => Dynamic t Bool
   -> Dynamic t RsvpEvent
   -> m (El t)
eventEl sel rsvp_event = do
  let commonAttrs = constDyn $ "class" =: "panel panel-default event-wrap"
  let attrs = fmap (`Common.monoidGuard` selectedStyle) sel
  (e,_) <- elDynAttr' "div" (zipDynWith classMerge attrs commonAttrs) $ do
    elClass "div" "panel-heading" $ dynText $ fmap eventName rsvp_event
    elClass "div" "panel-body" $ dynText $ fmap eventContact rsvp_event
    elClass "div" "img-wrapper" $ dyn $ fmap imgBinEl rsvp_event
  pure e

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

imgBinEl :: (MonadWidget t m) => RsvpEvent -> m ()
imgBinEl rsvp_event = do
  let attrs = eventImageAttrs $ def { ei_img_src = imgSrcFrom rsvp_event }
  elAttr "img" attrs blank

eventImageAttrs :: EventImageConfig -> ElAttrs
eventImageAttrs conf =
  let w = ei_width conf
      h = ei_height conf
      src = fromMaybe (Widget.placeholderImageLocation w h) (ei_img_src conf)
  in
    mconcat [ "src" =: src
            , "width" =: show w
            , "height" =: show h ]

imgSrcFrom :: RsvpEvent -> Maybe Text
imgSrcFrom rsvp_event = case eventImage rsvp_event of
  Nothing -> Nothing
  Just i -> Just $ "data:image/jpg;base64," <> encodeToText i

selectedStyle :: Map Text Text
selectedStyle = "class" =: "selected"

noDisplay :: Map Text Text
noDisplay = "style" =: "display: none"

blockDisplay :: Map Text Text
blockDisplay = "style" =: "display: block"
