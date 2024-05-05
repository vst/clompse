{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides definitions for Hetzner API connection
-- configuration.
module Clompse.Providers.Hetzner.Connection where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Hetzner.Cloud as Hetzner


-- | Data definition for Hetzner API connection configuration.
--
-- >>> Aeson.encode $ HetznerConnection "my-token"
-- "{\"token\":\"my-token\"}"
-- >>> Aeson.decode "{\"token\":\"my-token\"}" :: Maybe HetznerConnection
-- Just (HetznerConnection {_hetznerConnectionToken = "my-token"})
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


-- | Extracts the Hetzner API token from a connection configuration
-- and builds a 'Hetzner.Token' value.
--
-- >>> hetznerConnectionToken (HetznerConnection "my-token")
-- Token "my-token"
hetznerConnectionToken :: HetznerConnection -> Hetzner.Token
hetznerConnectionToken =
  Hetzner.Token . TE.encodeUtf8 . _hetznerConnectionToken
