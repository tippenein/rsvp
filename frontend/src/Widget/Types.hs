{-# LANGUAGE OverloadedStrings #-}
module Widget.Types where

import Data.Default
import Data.Map (Map)
import Data.Text (Text)

data FormGroupConfig
  = FormGroupConfig
  { _labelDescription :: Maybe Text
  , _formType :: Text
  , _placeholder :: Maybe Text
  , _helpAttrs :: Maybe (Map Text Text)
  , _helpText :: Maybe Text
  }

instance Default FormGroupConfig where
  def = FormGroupConfig
    { _labelDescription = Nothing
    , _formType = "text"
    , _placeholder = Nothing
    , _helpAttrs = Nothing
    , _helpText = Nothing
    }
