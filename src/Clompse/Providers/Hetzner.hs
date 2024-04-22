{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Clompse.Providers.Hetzner where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)


newtype HetznerConnection = HetznerConnection
  { _hetznerConnectionToken :: T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec HetznerConnection)


instance ADC.HasCodec HetznerConnection where
  codec =
    _codec ADC.<?> "Hetzner Connection"
    where
      _codec =
        ADC.object "HetznerConnection" $
          HetznerConnection
            <$> ADC.requiredField "token" "Hetzner API token." ADC..= _hetznerConnectionToken
