{-# LANGUAGE OverloadedStrings #-}
module Common where

import qualified Data.Text as T
import qualified Data.Map as Map
import           Data.Monoid
import Reflex.Dom

import Protolude
import Prelude()

maybeLookup :: Maybe Int -> Map Int a -> Maybe a
maybeLookup midx m = case midx of
  Nothing -> Nothing
  Just idx -> Map.lookup idx m

hsl :: Int -> Int -> Int -> Text
hsl h s l = "hsl(" <> inner <> ")"
  where
    inner = T.intercalate "," [show h, s',l']
    s' = show s <> "%"
    l' = show l <> "%"

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty

type ElAttrs = Map Text Text

btn :: DomBuilder t m => Text -> m (Event t ())
btn t = do
  (e, _) <- elAttr' "button" ("class" =: "btn btn-default") $ text t
  return $ domEvent Click e

btnClass :: DomBuilder t m => Text -> Text -> m (Event t ())
btnClass t c = btnAttr t ("class" =: c)

btnAttr :: DomBuilder t m => Text -> ElAttrs -> m (Event t ())
btnAttr t attrs = do
  (e, _) <- elAttr' "button" attrs $ text t
  return $ domEvent Click e

btnDynAttr :: MonadWidget t m => Dynamic t ElAttrs -> m (Event t ())
btnDynAttr attrs = do
  (e, _) <- elDynAttr' "input" attrs blank
  return $ domEvent Click e
