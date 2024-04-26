{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Clompse.Config where

import qualified Autodocodec as ADC
import Clompse.Providers.Aws (AwsConnection)
import Clompse.Providers.Do (DoConnection)
import Clompse.Providers.Hetzner (HetznerConnection)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Zamazingo.Text as Z.Text


data Config = Config
  { _configName :: !T.Text
  , _configCloudProfiles :: ![CloudProfile]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Config)


instance ADC.HasCodec Config where
  codec =
    _codec ADC.<?> "Application Configuration"
    where
      _codec =
        ADC.object "Config" $
          Config
            <$> ADC.optionalFieldWithDefault "name" "unnamed" "Name of the configuration." ADC..= _configName
            <*> ADC.optionalFieldWithDefault "cloud_profiles" [] "List of cloud profiles." ADC..= _configCloudProfiles


data CloudProfile = CloudProfile
  { _cloudProfileName :: !T.Text
  , _cloudProfileConnections :: ![CloudConnection]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec CloudProfile)


instance ADC.HasCodec CloudProfile where
  codec =
    _codec ADC.<?> "Cloud Profile"
    where
      _codec =
        ADC.object "CloudProfile" $
          CloudProfile
            <$> ADC.requiredField "name" "Name of the cloud profile." ADC..= _cloudProfileName
            <*> ADC.requiredField "connections" "Cloud API connection details." ADC..= _cloudProfileConnections


data CloudConnection
  = CloudConnectionAws !AwsConnection
  | CloudConnectionDo !DoConnection
  | CloudConnectionHetzner !HetznerConnection
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec CloudConnection)


instance ADC.HasCodec CloudConnection where
  codec = ADC.object "CloudConnection" ADC.objectCodec


instance ADC.HasObjectCodec CloudConnection where
  objectCodec = ADC.discriminatedUnionCodec "type" enc dec
    where
      codecCloudConnectionAws = ADC.requiredField "value" "AWS connection details"
      codecCloudConnectionDigitalOcean = ADC.requiredField "value" "DigitalOcean connection details"
      codecCloudConnectionHetzner = ADC.requiredField "value" "Hetzner connection details"
      enc x = case x of
        CloudConnectionAws c -> ("aws", ADC.mapToEncoder c codecCloudConnectionAws)
        CloudConnectionDo c -> ("do", ADC.mapToEncoder c codecCloudConnectionDigitalOcean)
        CloudConnectionHetzner c -> ("hetzner", ADC.mapToEncoder c codecCloudConnectionHetzner)
      dec =
        HashMap.fromList
          [ ("aws", ("CloudConnectionAws", ADC.mapToDecoder CloudConnectionAws codecCloudConnectionAws))
          , ("do", ("CloudConnectionDigitalOcean", ADC.mapToDecoder CloudConnectionDo codecCloudConnectionDigitalOcean))
          , ("hetzner", ("CloudProfile", ADC.mapToDecoder CloudConnectionHetzner codecCloudConnectionHetzner))
          ]


-- | Attempts to read a configuration file and return 'Config'.
--
-- Note that this function does not check if the file exists or is
-- readable (TODO).
readConfigFile :: FilePath -> IO (Either T.Text Config)
readConfigFile fp = do
  res <- Yaml.decodeFileEither fp
  case res of
    Left err -> pure (Left (Z.Text.tshow err))
    Right sv -> pure (Right sv)
