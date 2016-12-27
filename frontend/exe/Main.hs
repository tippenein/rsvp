{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Reflex.Dom

import qualified Widget
import qualified Lib

main :: IO ()
main = mainWidgetWithHead ( Widget.headElement "rsvp" ) (Lib.bodyElement)
