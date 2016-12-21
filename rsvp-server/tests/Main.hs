module Main
  ( main
  ) where

import Protolude

import Control.Monad.Log (Severity(..))
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Servant.QuickCheck
       ((<%>), createContainsValidLocation, defaultArgs, not500,
        notLongerThan, serverSatisfies,
        unauthorizedContainsWWWAuthenticate, withServantServer)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec (Spec, it, testSpec)

import Rsvp.API (api, ContactInfo(..), Event(..), Rsvp(..), User(..))
import Rsvp.Server (server)

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  specs <- testSpec "quickcheck tests" spec
  pure $ testGroup "Rsvp.Server" [specs]

instance Arbitrary ContactInfo where
  arbitrary = Phone <$> arbitrary

instance Arbitrary Event where
  arbitrary = Event <$> pos <*> arbitrary <*> arbitrary
     where pos = getPositive <$> arbitrary

instance Arbitrary Rsvp where
  arbitrary = Rsvp <$> pos <*> arbitrary <*> arbitrary
     where pos = getPositive <$> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary <*> arbitrary

spec :: Spec
spec =
  it "follows best practices" $
  withServantServer api (pure (server Error)) $
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
