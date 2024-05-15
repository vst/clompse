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
import qualified Hetzner.DNS as Hetzner.Dns


-- | Data definition for Hetzner API connection configuration.
--
-- >>> Aeson.encode $ HetznerConnection "my-token" Nothing
-- "{\"token\":\"my-token\",\"token_dns\":null}"
-- >>> Aeson.decode "{\"token\":\"my-token\"}" :: Maybe HetznerConnection
-- Just (HetznerConnection {_hetznerConnectionToken = "my-token", _hetznerConnectionTokenDns = Nothing})
data HetznerConnection = HetznerConnection
  { _hetznerConnectionToken :: !T.Text
  , _hetznerConnectionTokenDns :: !(Maybe T.Text)
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
            <*> ADC.optionalFieldWithDefault "token_dns" Nothing "Hetzner DNS API token." ADC..= _hetznerConnectionTokenDns


-- | Extracts the Hetzner API token from a connection configuration
-- and builds a 'Hetzner.Token' value.
--
-- >>> hetznerConnectionToken (HetznerConnection "my-token" Nothing)
-- Token "my-token"
hetznerConnectionToken :: HetznerConnection -> Hetzner.Token
hetznerConnectionToken =
  Hetzner.Token . TE.encodeUtf8 . _hetznerConnectionToken


-- | Extracts the Hetzner API token from a connection configuration
-- and builds a 'Hetzner.Token' value (for DNS API).
--
-- >>> hetznerConnectionTokenDns (HetznerConnection "my-token" Nothing)
-- Nothing
-- >>> hetznerConnectionTokenDns (HetznerConnection "my-token" (Just "my-dns-token"))
-- Just (Token "my-dns-token")
hetznerConnectionTokenDns :: HetznerConnection -> Maybe Hetzner.Dns.Token
hetznerConnectionTokenDns =
  fmap (Hetzner.Dns.Token . TE.encodeUtf8) . _hetznerConnectionTokenDns
