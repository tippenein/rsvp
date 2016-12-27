
module Common where
-- import           Data.Text (Text)
-- import qualified Data.Text as T
import           Data.Monoid

import Protolude

-- hsl :: Int -> Int -> Int -> Text
-- hsl h s l = "hsl(" <> inner <> ")"
--   where
--     inner = intercalate "," [show h, s',l']
--     s' = show s <> "%"
--     l' = show l <> "%"

monoidGuard :: Monoid a => Bool -> a -> a
monoidGuard p a = if p then a else mempty
