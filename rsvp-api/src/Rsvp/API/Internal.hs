-- | Definition of HTML content type.
module Rsvp.API.Internal
  ( HTML
  ) where

import Network.HTTP.Media ((//), (/:))
import Servant.API (Accept(..))

-- | HTML content type.
data HTML

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")
