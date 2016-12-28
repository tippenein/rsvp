{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Component where

import           Control.Lens hiding (view, element)
import qualified Data.Map as Map
import           Database.Persist.Sql (toSqlKey)
import           GHCJS.DOM.Types (File)
import           Reflex.Dom
------------------------------------
import           Common
import qualified Shared.Types as Shared
import qualified Widget
import           Shared.Types hiding (Event)
import           Request

import           Protolude hiding (div, (&), ByteString)
import           Prelude ()
------------------------------------

eventForm :: MonadWidget t m => Dynamic t Bool -> m (Event t EventCreateResponse, Event t (), Event t ())
eventForm visible = do
  let attrs' = fmap (`Common.monoidGuard` blockDisplay ) visible
  let attrs = zipDynWith Map.union attrs' (constDyn noDisplay)
  elDynAttr "div" attrs $ do
    (eventSubmitData, submitEvent, cancelEvent) <- mkEventForm
    let postData = Request.toPostWithEncode (defaultUrl EventsRoute) <$> eventSubmitData
    createRsp :: Event t XhrResponse <- performRequestAsync $ attachPromptlyDynWith const postData submitEvent
    let eventCreateResponse :: Event t EventCreateResponse = fmapMaybe decodeXhrResponse createRsp
    pure (eventCreateResponse, submitEvent, cancelEvent)

mkEventForm :: MonadWidget t m => m (Dynamic t RsvpEvent, Event t (), Event t ())
mkEventForm = do
  (submitEvent, cancelEvent, nameDyn, contactDyn) <- el "form-inline" $ do
    nameDyn <- formGroup "text" "eventName" "Event Name"
    contactDyn <- formGroup "text" "contactInfo" "Contact Info"
    files <- bootstrapFileInput

    cancelButton <- btn "cancel"
    submitButton <- btnClass "submit" "btn btn-primary"
    el "hr" blank
    pure (submitButton, cancelButton, nameDyn, contactDyn)

  let e = Shared.Event (toSqlKey 1) <$> nameDyn <*> contactDyn
  pure (e, submitEvent, cancelEvent)

bootstrapFileInput :: MonadWidget t m => m (Dynamic t [File])
bootstrapFileInput = do
  text "Event Image"
  a <- elClass "label" "btn btn-default btn-file" $ do
    f <- fileInput def
    pure f
  pure $ _fileInput_value a

eventListing :: MonadWidget t m => Dynamic t (Map Int RsvpEvent) -> Dynamic t (Maybe Int) -> m (Event t Int)
eventListing eventMap selectedEvent = do
  eventSelected <- divClass "row" $ do
    bs' <- divClass "event-list"$ do
      bs <- elClass "ul" "list-unstyle" $ Widget.selectableList selectedEvent eventMap $ \sel p ->
        domEvent Click <$> Component.eventEl sel p
      pure bs
    -- display $ zipDynWith maybeLookup selectedEvent eventMap
    pure bs'
  pure eventSelected

showStatus :: MonadWidget t m => Maybe Status -> m ()
showStatus (Just (Shared.Success t)) = text t
showStatus (Just (Shared.Warning t)) = text t
showStatus (Just (Shared.Info t)   ) = text t
showStatus (Just (Shared.Error t)  ) = text t
showStatus Nothing = blank

type FormInputType = Text

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
   -> m(El t)
eventEl sel b = do
  let commonAttrs = constDyn $ "class" =: "event-wrap"
  let attrs = fmap (`Common.monoidGuard` selectedStyle) sel
  (e,_) <- elDynAttr' "li" (zipDynWith classMerge attrs commonAttrs) $ do
    dynText $ fmap eventName b
    text " - "
    dynText $ fmap eventContact b
  pure e

classMerge :: ElAttrs -> ElAttrs -> ElAttrs
classMerge a b = Map.fromListWith (\a' b' -> a' <> " " <> b') $ Map.toList a ++ Map.toList b

selectedStyle :: Map Text Text
selectedStyle = "class" =: "selected"

noDisplay :: Map Text Text
noDisplay = "style" =: "display: none"

blockDisplay :: Map Text Text
blockDisplay = "style" =: "display: block"
