-- | Launch rsvp server.
module Main
  ( main
  ) where

import Protolude

import Rsvp.Server (startApp)

main :: IO ()
main = startApp
