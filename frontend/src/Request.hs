{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Request where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString, toStrict)
import           Data.Text.Encoding (decodeUtf8)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Network.HTTP.Types.URI
------------------------------------------------------------------------------
import           Reflex
import Reflex.Dom

import qualified Shared.Types as Shared
import Protolude hiding (ByteString)
import Prelude ()


data Route
  = EventsRoute
  | EventRoute Shared.DbKey
  | UsersRoute
  | UserRoute Shared.DbKey
  deriving (Show, Eq)

mkGET :: Route -> Maybe (Map Text Text)-> XhrRequest ()
mkGET u Nothing = XhrRequest "GET" (defaultUrl u) def
mkGET u (Just qp) = XhrRequest "GET" uri def
  where
    params = T.intercalate "&" $ map (\(k,v) -> k <> "=" <> v) $ Map.toList qp
    uri = defaultUrl u <> "?" <> params

defaultUrl :: Route -> Text
defaultUrl EventsRoute = "http://localhost:8081/events"
defaultUrl (EventRoute i) = "http://localhost:8081/events/" <> Protolude.show i
defaultUrl UsersRoute = "http://localhost:8081/users"
defaultUrl (UserRoute i)= "http://localhost:8081/users" <> Protolude.show i

toPostWithEncode :: (ToJSON a) => Text -> a -> XhrRequest Text
toPostWithEncode url j =
  let d = decodeUtf8 $ toStrict $ encode j
      headerEnc = "Content-type" =: "application/json"
  in XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerEnc
                                 , _xhrRequestConfig_sendData = d
                                 }

toPost :: Text -> Text -> XhrRequest Text
toPost url d =
    XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                                , _xhrRequestConfig_sendData = d
                                }
  where
    headerUrlEnc :: Map Text Text
    headerUrlEnc = "Content-type" =: "application/x-www-form-urlencoded"

formEncodeJSON :: ToJSON a => a -> Text
formEncodeJSON a = case toJSON a of
  Object m ->
    formEncode $ Map.fromList $ map (bimap identity encode) $ itoList m
  _ -> error "formEncodeJSON requires an Object"

-- | URL encodes a map of key-value pairs.
formEncode :: Map Text ByteString -> Text
formEncode m =
  T.intercalate "&" $
    map (\(k,v) -> k <> "=" <> encodeToText v) $ Map.toList m
  where
    encodeToText :: ByteString -> Text
    encodeToText = toS . urlEncode True . toS
-- | This is the foundational primitive for the XHR API because it gives you
-- full control over request generation and response parsing and also allows
-- you to match things that generated the request with their corresponding
-- responses.
performAJAX
    :: (MonadWidget t m, IsXhrPayload a)
    => (a -> XhrRequest a) -- ^ Function to build the request
    -> (XhrResponse -> b) -- ^ Function to parse the response
    -> Event t a
    -> m (Event t (a, b))
performAJAX mkRequest parseResponse req =
  performEventAsync $ ffor req $ \a cb -> do
    _ <- newXMLHttpRequest (mkRequest a) $ \response ->
            liftIO $ cb (a, parseResponse response)
    return ()


------------------------------------------------------------------------------
-- | Performs an async XHR taking a JSON object as input and another JSON
-- object as output.
performJsonAjax
    :: (MonadWidget t m, ToJSON a, FromJSON b)
    => Event t (Text, a)
    -- ^ Event with a URL and a JSON object to be sent
    -> m (Event t (a, Maybe b))
performJsonAjax req =
  performEventAsync $ ffor req $ \(url,a) cb -> do
    _ <- newXMLHttpRequest (mkRequest url a) $ \response ->
            liftIO $ cb (a, decodeXhrResponse response)
    return ()
  where
    mkRequest url a = toPost url (formEncodeJSON a)

