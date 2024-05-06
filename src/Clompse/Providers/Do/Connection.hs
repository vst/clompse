{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions for DigitalOcean API connection
-- configuration.
module Clompse.Providers.Do.Connection where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)


-- | Data definition for DigitalOcean API connection configuration.
--
-- >>> Aeson.encode $ DoConnection "my-token" Nothing Nothing
-- "{\"token\":\"my-token\"}"
-- >>> Aeson.decode "{\"token\":\"my-token\"}" :: Maybe DoConnection
-- Just (DoConnection {_doConnectionToken = "my-token", _doConnectionSpacesAccessKeyId = Nothing, _doConnectionSpacesSecretAccessKey = Nothing})
data DoConnection = DoConnection
  { _doConnectionToken :: !T.Text
  , _doConnectionSpacesAccessKeyId :: !(Maybe T.Text)
  , _doConnectionSpacesSecretAccessKey :: !(Maybe T.Text)
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoConnection)


instance ADC.HasCodec DoConnection where
  codec =
    _codec ADC.<?> "DigitalOcean Connection"
    where
      _codec =
        ADC.object "DigitalOceanConnection" $
          DoConnection
            <$> ADC.requiredField "token" "DigitalOcean API token." ADC..= _doConnectionToken
            <*> ADC.optionalField "spaces_access_key_id" "DigitalOcean Spaces access key identifier." ADC..= _doConnectionSpacesAccessKeyId
            <*> ADC.optionalField "spaces_secret_access_key" "DigitalOcean Spaces secret access key." ADC..= _doConnectionSpacesSecretAccessKey
