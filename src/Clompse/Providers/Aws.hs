{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Clompse.Providers.Aws where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import GHC.Generics (Generic)


data AwsConnection = AwsConnection
  { _awsConnectionAccessKeyId :: !T.Text
  , _awsConnectionSecretAccessKey :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec AwsConnection)


instance ADC.HasCodec AwsConnection where
  codec =
    _codec ADC.<?> "AWS Connection"
    where
      _codec =
        ADC.object "AwsConnection" $
          AwsConnection
            <$> ADC.requiredField "access_key_id" "AWS access key ID." ADC..= _awsConnectionAccessKeyId
            <*> ADC.requiredField "secret_access_key" "AWS secret access key." ADC..= _awsConnectionSecretAccessKey
