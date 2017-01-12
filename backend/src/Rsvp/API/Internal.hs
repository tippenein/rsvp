{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- | Definition of HTML content type.
module Rsvp.API.Internal
  ( HTML
  , SelectById
  ) where

import Network.HTTP.Media ((//), (/:))
import Servant.API
import Shared.Types

-- | HTML content type.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

type SelectById field record =
     Capture field DbKey
  :> Get '[JSON] record

