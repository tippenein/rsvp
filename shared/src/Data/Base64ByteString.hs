{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE RankNTypes #-}

module Data.Base64ByteString where

import Data.Aeson
import           Data.Binary (Binary)
import Data.ByteString
import qualified Data.ByteString.Base64 as Base64
import           Data.Data (Data)
import Data.Text.Encoding as T
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import           Data.String (IsString (..))
import GHC.Generics

import Protolude
-- | Aeson serialisable bytestring. Uses base64 encoding.
newtype ByteString64 = ByteString64 { getByteString64 :: ByteString }
    deriving (Eq, Read, Show, Ord, Data, Typeable, Generic, Hashable, Serialize, Binary)

-- | Get base64 encode bytestring
getEncodedByteString64 :: ByteString64 -> ByteString
getEncodedByteString64 = Base64.encode . getByteString64

instance ToJSON ByteString64 where
    toJSON = toJSON . T.decodeLatin1 . getEncodedByteString64

instance FromJSON ByteString64 where
    parseJSON = withText "ByteString" $
        pure . ByteString64 . Base64.decodeLenient . T.encodeUtf8

instance IsString ByteString64 where
   fromString = ByteString64 . fromString
