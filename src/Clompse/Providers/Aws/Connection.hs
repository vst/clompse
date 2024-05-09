{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides definitions for AWS API connection
-- configuration.
module Clompse.Providers.Aws.Connection where

import qualified Amazonka as Aws
import qualified Amazonka.Auth as Aws.Auth
import qualified Autodocodec as ADC
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)


-- | Data definition for AWS API connection configuration.
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


-- | Create AWS API environment from connection configuration.
_envFromConnection
  :: MonadIO m
  => AwsConnection
  -> m Aws.Env
_envFromConnection AwsConnection {..} =
  Aws.newEnv (pure . Aws.Auth.fromKeys accessKeyId secretAccessKey)
  where
    accessKeyId = Aws.AccessKey (TE.encodeUtf8 _awsConnectionAccessKeyId)
    secretAccessKey = Aws.SecretKey (TE.encodeUtf8 _awsConnectionSecretAccessKey)
