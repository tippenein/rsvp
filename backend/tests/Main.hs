{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
  ( main
  ) where

import qualified Control.Monad.Log as Log
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Servant.QuickCheck
       ((<%>), createContainsValidLocation, defaultArgs, not500,
        notLongerThan, serverSatisfies,
        unauthorizedContainsWWWAuthenticate, withServantServer)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec -- (Spec, it, testSpec)
import System.IO.Unsafe (unsafePerformIO)
--------------------------------------------
import Rsvp.API (rsvpApi)
import Shared.Types
import Database.Persist.Sql
import Rsvp.Server.Models
import Rsvp.Server.Config (Config(..), makePool, Environment(Test))
import Rsvp.Server.Handlers (rsvpServer)

import Protolude

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  env <- pure Test
  let cfg  = Config (unsafePerformIO $ makePool Test) Log.Error env
  liftIO $ setupTest cfg
  specs <- testSpec "quickcheck tests" $ spec cfg
  units <- testSpec "unit tests" unitTests
  pure $ testGroup "Rsvp.Backend" [specs, units]

-- arbitraryPositiveInt :: (Num a, Ord a, Arbitrary a) => Gen a
-- arbitraryPositiveInt = arbitrary `suchThat` (> 1)

someId :: Gen Int64
someId = pure 1

instance Arbitrary Event where
  arbitrary = Event <$> (toKey <$> someId) <*> arbitrary <*> arbitrary

instance Arbitrary Rsvp where
  arbitrary = Rsvp <$> (toKey <$> someId) <*> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

spec :: Config -> Spec
spec cfg = do
  it "follows best practices" $
    withServantServer rsvpApi (pure $ rsvpServer cfg) $
      \burl ->
        serverSatisfies
          rsvpApi
          burl
          defaultArgs
          (not500 <%>
            createContainsValidLocation <%>
            notLongerThan 100000000 <%>
            unauthorizedContainsWWWAuthenticate <%>
            mempty)

setupTest :: Config -> IO ()
setupTest cfg = runSqlPool stuff (getPool cfg)
  where
    stuff = do
      doMigrations
      uid <- insert $ User "whatever" "whatever@whatever.com"
      _ <- insert $ Event uid "some event" "derp"
      pure()

unitTests :: Spec
unitTests =
  it "passes" $
    True `shouldBe` True

--   [ testCase "List comparison (different length)" $
--       [1, 2, 3] `compare` [1,2] @?= GT

--   -- the following test does not hold
--   , testCase "List comparison (same length)" $
--       [1, 2, 3] `compare` [1,2,2] @?= LT
--   ]
