{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main
  ( main
  ) where

import qualified Control.Monad.Log as Log
-- import Data.Aeson
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Servant.QuickCheck
       ((<%>), createContainsValidLocation, defaultArgs, not500,
        notLongerThan, serverSatisfies,
        unauthorizedContainsWWWAuthenticate, withServantServer)
import Test.Tasty (defaultMain, TestTree, testGroup)
import Test.Tasty.Hspec -- (Spec, it, testSpec)
-- import Test.Tasty.QuickCheck as QC
import System.IO.Unsafe (unsafePerformIO)
--------------------------------------------
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as ByteString
import Rsvp.API (rsvpApi)
import Shared.Types
import Database.Persist.Sql
import Rsvp.Server.Models
import Rsvp.Server.Config (Config(..), makePool, Environment(Test), mkAuthConfig)
import Rsvp.Server.Handlers (rsvpServer)

import Protolude

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  env <- pure Test
  let pool = unsafePerformIO $ makePool Test
  let cfg  = Config pool Log.Error (mkAuthConfig pool) env
  liftIO $ setupTest cfg
  specs <- testSpec "servant tests" $ spec cfg
  units <- testSpec "unit tests" unitTests
  pure $ testGroup "Rsvp.Backend" [specs, sharedSpec, units]


someId :: Gen Int64
someId = pure 1

instance Arbitrary Event where
  arbitrary = Event <$> (toKey <$> someId) <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Rsvp where
  arbitrary = Rsvp <$> (toKey <$> someId) <*> arbitrary <*> arbitrary

instance Arbitrary User where
  arbitrary = User <$> arbitrary <*> arbitrary

spec :: Config -> Spec
spec cfg =
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

sharedSpec :: TestTree
sharedSpec = testGroup "encoding/decoding" [
  -- QC.testProperty "encoding . decoding  event" $
  --     \e -> encode $ decode $ (e :: Event) == e
  ]

setupTest :: Config -> IO ()
setupTest cfg = runSqlPool stuff (getPool cfg)
  where
    stuff = do
      doMigrations
      uid <- insert $ User "whatever" "whatever@whatever.com"
      let img = unsafePerformIO $ B64.encode <$> ByteString.readFile "tests/fixtures/image.png"
      _ <- insert $ Event uid "some event" "derp" (Just img)
      _ <- insert $ Event uid "some other event" "something" Nothing
      pure()

unitTests :: Spec
unitTests =
  it "passes" $
    True `shouldBe` True

-- arbitraryPositiveInt :: (Num a, Ord a, Arbitrary a) => Gen a
-- arbitraryPositiveInt = arbitrary `suchThat` (> 1)
