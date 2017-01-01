{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component where

import           Control.Lens hiding (view, element)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Database.Persist.Sql (toSqlKey)
import           GHCJS.DOM.Types (File)
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
                      , "action" =: "/events"
                      , "method" =: "post"
                      -- , "data-remote" =: "true"
                      ]
  (submitEvent, cancelEvent, nameDyn, contactDyn) <- elAttr "form" attrs $ do
    nameDyn <- formGroup "text" "eventName" "Event Name"
    contactDyn <- formGroup "text" "contactInfo" "Contact Info"
    files <- bootstrapFileInput

    cancelButton <- btn "cancel"
    submitButton <- btnClass "submit" "btn btn-primary"
    pure (submitButton, cancelButton, nameDyn, contactDyn)

  let e = Shared.Event (toSqlKey 1) <$> nameDyn <*> contactDyn
  pure (e, submitEvent, cancelEvent)

bootstrapFileInput :: MonadWidget t m => m (Dynamic t [File])
bootstrapFileInput = do
  text "Event Image"
  a <- elClass "label" "btn btn-default btn-file" $
    pure =<< fileInput def
  pure $ _fileInput_value a

eventListing :: MonadWidget t m => Dynamic t (Map DbKey RsvpEvent) -> Dynamic t (Maybe DbKey) -> m (Event t DbKey)
eventListing eventMap selectedEvent = do
  eventSelected <- divClass "row" $
    elClass "div" "event-listing" $ Widget.selectableList selectedEvent eventMap $ \sel p ->
      domEvent Click <$> eventEl sel p
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

-- XXX: use a FormGroupConfig to set up all possible options
formGroup :: MonadWidget t m
          => Text -- email | text | phone
          -> Text -- id
          -> Text -- full descript
          -> m (Dynamic t Text)
formGroup t i desc = do
  v <- divClass "form-group" $ do
    -- elAttr "label" ("for" =: i) $ text desc
    let attrs = constDyn $ mconcat [ "type" =: t
                     , "class" =: "form-control"
                     , "id" =: i
                     -- , "aria-describedby" =: "emailHelp"
                     , "placeholder" =: desc
                     ]
    g <- textInput $ def & textInputConfig_attributes .~ attrs
    -- elAttr "small" ("id" =: "emailHelp" <> "class" =: "form-text text-muted") $ text "We'll never share your email with anyone else."
    pure g
  pure $ _textInput_value v

eventEl :: (MonadWidget t m)
   => Dynamic t Bool
   -> Dynamic t RsvpEvent
   -> m (El t)
eventEl sel b = do
  let commonAttrs = constDyn $ "class" =: "panel panel-default event-wrap"
  let attrs = fmap (`Common.monoidGuard` selectedStyle) sel
  (e,_) <- elDynAttr' "div" (zipDynWith classMerge attrs commonAttrs) $ do
    elClass "div" "panel-heading" $ dynText $ fmap eventName b
    elClass "div" "panel-body" $ dynText $ fmap eventContact b
    Widget.placeholderImage 300 300
  pure e

selectedStyle :: Map Text Text
selectedStyle = "class" =: "selected"

noDisplay :: Map Text Text
noDisplay = "style" =: "display: none"

blockDisplay :: Map Text Text
blockDisplay = "style" =: "display: block"
