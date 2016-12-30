module Main
  ( main
  ) where

import Protolude

import Protolude

import Control.Monad.Log hiding (Error)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Servant.QuickCheck
       ((<%>), createContainsValidLocation, defaultArgs, not500,
        notLongerThan, serverSatisfies,
        unauthorizedContainsWWWAuthenticate, withServantServer)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, testSpec)
import Database.Persist.Sql (toSqlKey)

import Rsvp.API (api)
import Shared.Types
import Rsvp.Server.Models
import Rsvp.Server (server)

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  specs <- testSpec "quickcheck tests" spec
  pure $ testGroup "Rsvp.Backend" [specs]

-- instance Arbitrary ContactInfo where
--   arbitrary = Phone <$> arbitrary

-- key = toSqlKey $ getPositive <$> arbitrary

-- instance Arbitrary Event where
--   arbitrary = Event <$> key <*> arbitrary <*> arbitrary

-- instance Arbitrary Rsvp where
--   arbitrary = Rsvp <$> key <*> arbitrary <*> arbitrary

-- instance Arbitrary User where
--   arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary

cfg :: Config
cfg = def { logLevel = Error }

spec :: Spec
spec =
  it "follows best practices" $
  withServantServer api (pure (server cfg)) $
  \burl ->
     serverSatisfies
       api
       burl
       defaultArgs
       (not500 <%>
        createContainsValidLocation <%>
        notLongerThan 100000000 <%>
        unauthorizedContainsWWWAuthenticate <%>
        mempty)

