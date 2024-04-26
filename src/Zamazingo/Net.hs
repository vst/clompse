{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Zamazingo.Net where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Net.IPv4
import qualified Net.IPv6


newtype IPv4 = MkIPv4
  { _unIPv4 :: Net.IPv4.IPv4
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec IPv4)


instance ADC.HasCodec IPv4 where
  codec = ADC.named _type _codec ADC.<?> _docs
    where
      _type = "IPv4"
      _docs = "An IPv4 address"
      _codec = ADC.dimapCodec MkIPv4 _unIPv4 (ADC.codecViaAeson "_IPv4")


newtype IPv6 = MkIPv6
  { _unIPv6 :: Net.IPv6.IPv6
  }
  deriving (Eq, Ord, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec IPv6)


instance ADC.HasCodec IPv6 where
  codec = ADC.named _type _codec ADC.<?> _docs
    where
      _type = "IPv6"
      _docs = "An IPv6 address"
      _codec = ADC.dimapCodec MkIPv6 _unIPv6 (ADC.codecViaAeson "_IPv6")
