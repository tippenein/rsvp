{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Widget where

import           Reflex
import           Reflex.Dom

import qualified Data.Text as T
import           Data.Default (Default)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (defaultTimeLocale, parseTimeM)

import           Common
import           Protolude hiding ((&))

type PageTitle = Text

searchInput :: MonadWidget t m => m (TextInput t)
searchInput =
  textInput $
    def & attributes .~ constDyn
      (mconcat [ "class" =: "search"
               , "placeholder" =: "Search.."
               ]
      )
headElement :: MonadWidget t m => Text -> m ()
headElement title = do
  el "title" $ text title
  stylesheetImports
  pure ()

headElementDyn :: MonadWidget t m => Dynamic t Text -> m ()
headElementDyn title = do
  _ <- elDynHtml' "title" title
  stylesheetImports
  pure ()

stylesheetImports :: MonadWidget t m => m ()
stylesheetImports = do
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  styleSheet "http://fonts.googleapis.com/css?family=Lato"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/twitter-bootstrap/4.0.0-alpha.5/css/bootstrap.min.css"
  styleSheet "css/style.css"
  where
    styleSheet _link = elAttr "link" (Map.fromList [
          ("rel", "stylesheet")
        , ("type", "text/css")
        , ("href", _link)
      ]) $ pure ()

readableInput :: (MonadWidget t m, Read a) => TextInputConfig t -> m (Event t a)
readableInput conf = do
    c <- textInput conf
    pure $ fmapMaybe readMaybe $ T.unpack <$> _textInput_input c


-- buttonWith :: DomBuilder t m => String -> Map.Map String String -> m (Event t ())
buttonWith title attrs = do
  (e,_) <- elAttr' "a" attrs $ text title
  pure $ domEvent Click e

-- buttonWithDyn :: DomBuilder t m => String -> Dynamic t (Map.Map String String) -> m (Event t ())
buttonWithDyn title attrs = do
  (e,_) <- elDynAttr' "button" attrs $ text title
  pure $ domEvent Click e

maybeButton :: MonadWidget t m
            => Dynamic t Bool -- ^ Is the button enabled?
            -> Text -- ^ Static button label
            -> m (Event t ())
maybeButton enabled label = do
    let attrs = ffor enabled $ \e -> monoidGuard (not e) $ "disabled" =: "disabled"
    (btn, _) <- elDynAttr' "button" attrs $ text label
    pure $ domEvent Click btn

datePicker :: MonadWidget t m
           => Dynamic t Bool -- ^ Widget enabled?
           -> m (Dynamic t (Maybe UTCTime))
datePicker enabled = do
    rec raw <- textInput $ def & textInputConfig_attributes .~ attrs
        attrs <- dynCombine date enabled $ \d e ->
            monoidGuard (isNothing d) ("style" =: "color: red") <>
            monoidGuard (not e) ("disabled" =: "disabled")
        let date = fmap (parseTimeM True defaultTimeLocale "%F") $ T.unpack <$> _textInput_value raw
    return date

selectableList :: (MonadWidget t m, Ord k)
               => Dynamic t (Maybe k)
               -- ^ Key of element that may be selected
               -> Dynamic t (Map k v)
               -- ^ Map of elements to be shown in the list
               -> (Dynamic t Bool -> Dynamic t v -> m (Event t a))
               -- ^ Action that renders a widget for an element. The element may fire events
               -> m (Event t k)
               -- ^ List fires events whenever an element is selected
selectableList selection elems mkEntry = do
  selectEntry <- listWithKey elems $ \k v -> do
      let isSelected = ffor selection $ \s -> s == Just k
      fmap (const k) <$> mkEntry isSelected v
  let selected = fmap (leftmost . Map.elems) selectEntry
  pure $ switchPromptlyDyn selected

dynCombine :: (Reflex t, MonadHold t m)
           => Dynamic t a -> Dynamic t b
           -> (a -> b -> c)
           -> m (Dynamic t c)
dynCombine a b f = pure (zipDynWith f a b)

dynCombine3 :: (Reflex t, MonadHold t m)
            => Dynamic t a -> Dynamic t b -> Dynamic t c
            -> (a -> b -> c -> d)
            -> m (Dynamic t d)
dynCombine3 da db dc f = do
  let dg = zipDynWith f da db
  pure $ zipDynWith (\g c -> g c) dg dc

checkboxAttrs :: forall b t.
  (Reflex t, Default b, HasAttributes b, Attrs b ~ Dynamic t (Map Text Text))
  => Text -- | name
  -> Text -- | value
  -> b
checkboxAttrs name v = def & attributes .~ constDyn (
                mconcat [ "name" =: name
                        , "id" =: name
                        , "type" =: "checkbox"
                        , "value" =: v
                        ]
                )
