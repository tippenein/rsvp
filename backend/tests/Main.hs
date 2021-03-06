{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Log as Log
import           Data.Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as B64
import           Database.Persist.Sql hiding (get)
import           Network.HTTP.Types.Method (methodPost)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai (Application)
import           Network.Wai.Test (SResponse)
import           Servant.Mock
import           Servant.QuickCheck
                  ((<%>), createContainsValidLocation, defaultArgs, not500,
                    notLongerThan, serverSatisfies,
                    unauthorizedContainsWWWAuthenticate, withServantServer)
import           Servant.Server (serve)
import           System.IO.Unsafe (unsafePerformIO)
import           Test.Hspec.Wai
-- import           Test.Hspec.Wai.JSON
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           Test.Tasty (defaultMain, TestTree, testGroup)
import           Test.Tasty.Hspec -- (Spec, it, testSpec)
-- import Test.Tasty.QuickCheck as QC
--------------------------------------------
import           Rsvp.API (rsvpApi)
-- import           Rsvp.API.Internal (HTML)
import           Shared.Types
import qualified Shared.Types as Types
import qualified Shared.Models as Model
import           Rsvp.Server.Models
import           Rsvp.Server.Config (Config(..), makePool, Environment(Test), mkAuthConfig)
import           Rsvp.Server.Handlers (rsvpServer)

import           Protolude hiding (get)

main :: IO ()
main = defaultMain =<< tests

tests :: IO TestTree
tests = do
  env <- pure Test
  let pool = unsafePerformIO $ makePool Test
  let cfg  = Config pool Log.Error (mkAuthConfig pool) env
  liftIO $ setupTest cfg
  specs <- testSpec "servant tests" $ spec cfg
  apiSpecs <- testSpec "api tests" $ apiSpec cfg
  units <- testSpec "unit tests" unitTests
  liftIO $ tearDownTest cfg
  pure $ testGroup "Rsvp.Backend" [sharedSpec, units, specs, apiSpecs]


instance Arbitrary (BackendKey SqlBackend) where
  arbitrary = SqlBackendKey <$> arbitraryPositiveInt

instance Arbitrary Status where
  arbitrary = arbitrary :: Gen Types.Status

instance (ToBackendKey SqlBackend a, Arbitrary a) => Arbitrary (CreateResponse a) where
  arbitrary = CreateResponse <$> arbitrary <*> arbitraryPositiveInt <*> arbitrary

instance Arbitrary Model.Event where
  arbitrary = Model.Event <$>
    (Model.UserKey <$> arbitraryPositiveInt)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Model.Rsvp where
  arbitrary = Model.Rsvp
    <$> (Model.EventKey <$> arbitraryPositiveInt)
    <*> arbitrary
    <*> arbitrary

instance Arbitrary Model.User where
  arbitrary = Model.User <$> arbitrary <*> arbitrary

instance (ToBackendKey SqlBackend a, Arbitrary a) => Arbitrary (Entity a) where
  arbitrary = do
    key <- toSqlKey <$> arbitrary
    whatever <- arbitrary
    pure $ Entity key whatever

instance (ToBackendKey SqlBackend a, Arbitrary a) => Arbitrary (PaginatedResponse a) where
  arbitrary = PaginatedResponse <$> arbitraryPositiveInt <*> arbitraryPositiveInt <*> arbitraryList

arbitraryList :: Arbitrary a => Gen [a]
arbitraryList =
  sized $
    \n -> do
      k <- choose (0, n)
      sequence [ arbitrary | _ <- [1..k] ]

arbitraryPositiveInt :: (Num a, Ord a, Arbitrary a) => Gen a
arbitraryPositiveInt = arbitrary `suchThat` (> 1)

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

app :: forall (f :: * -> *). Applicative f => Config -> f Application
app cfg = pure $ serve rsvpApi (rsvpServer cfg)

-- mockServer :: IO Application
-- mockServer = pure $ serve rsvpApi $ mock rsvpApi Proxy

apiSpec :: Config -> Spec
apiSpec cfg = with (setupTest cfg >> app cfg) $ do
  it "paginates" $
    get "/events?page=1&per_page=1" `shouldRespondWith` 200

  it "denies bad event with 405" $ do
    let e = Model.Event (toSqlKey 9001) "new" "something" Nothing Nothing Nothing
    postJson "/events" e `shouldRespondWith` 400

  it "fetches image from event id" $ do
    i <- liftIO $ runSqlPool stuff (getPool cfg)
    get ("/events/" <> show i <> "/image") `shouldRespondWith` 200
    where
      stuff = do
        uid <- insert $ Model.User "a" "b"
        i <- insert $ event_with_image uid
        pure $ fromSqlKey i

postJson :: (ToJSON a) => ByteString -> a -> WaiSession SResponse
postJson path = request methodPost path [(hContentType, "application/json")] . encode

sharedSpec :: TestTree
sharedSpec = testGroup "encoding/decoding" [
  -- QC.testProperty "encoding . decoding  event" $
  --   \e -> encode $ decode (generate (arbitrary :: Gen Event)) == e
  ]

setupTest :: Config -> IO ()
setupTest cfg = runSqlPool stuff (getPool cfg)
  where
    stuff = do
      doMigrations
      uid <- insert $ Model.User "whatever" "whatever@whatever.com"
      _ <- insert $ event_with_image uid
      _ <- insert $ Model.Event uid "no image" "something" Nothing Nothing Nothing
      pure()

tearDownTest :: Config -> IO ()
tearDownTest cfg = runSqlPool stuff (getPool cfg)
  where
    stuff = do
      deleteWhere [Model.EventId >=. toSqlKey 0]
      deleteWhere [Model.UserId >=. toSqlKey 0]
      pure()

event_with_image :: Key Model.User -> Model.Event
event_with_image uid =
  let img = unsafePerformIO $ B64.encode <$> ByteString.readFile "tests/fixtures/image.png"
  in Model.Event uid "with image" "derp" Nothing Nothing (Just img)

unitTests :: Spec
unitTests =
  it "passes" $
    True `shouldBe` True
