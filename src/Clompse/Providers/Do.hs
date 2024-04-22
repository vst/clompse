{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Clompse.Providers.Do where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)


newtype DoConnection = DoConnection
  { _doConnectionToken :: T.Text
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
