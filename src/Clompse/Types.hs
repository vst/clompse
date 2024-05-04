{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module provides top-level definitions for the types used
-- across the application.
module Clompse.Types where

import qualified Autodocodec as ADC
import qualified Data.Aeson as Aeson
import Data.Int (Int16, Int32)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Generics (Generic)
import qualified Zamazingo.Net as Z.Net


-- $setup
--
-- >>>:set -XOverloadedStrings
-- >>>:set -XTypeApplications


-- | Cloud Provider.
--
-- >>>Aeson.encode @[Provider] [minBound .. maxBound]
-- "[\"aws\",\"do\",\"hetzner\"]"
-- >>>Aeson.decode @[Provider] "[\"aws\",\"do\",\"hetzner\"]"
-- Just [ProviderAws,ProviderDo,ProviderHetzner]
data Provider
  = ProviderAws
  | ProviderDo
  | ProviderHetzner
  deriving (Bounded, Enum, Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Provider)


-- | 'ADC.HasCodec' instance for 'Provider'.
instance ADC.HasCodec Provider where
  codec =
    _codec ADC.<?> "Provider"
    where
      _codec = ADC.stringConstCodec (NE.fromList (fmap (\x -> (x, providerCode x)) [minBound .. maxBound]))


-- | Returns a code for a given 'Provider'.
providerCode :: Provider -> T.Text
providerCode ProviderAws = "aws"
providerCode ProviderDo = "do"
providerCode ProviderHetzner = "hetzner"


-- | Server state.
--
-- Each cloud provider has its own set of states, but we are using
-- this data definition to represent the common states. The states are
-- exhaustive, but not all of them are used by all providers.
--
-- The states are more or less ordered by their lifecycle, from the
-- initial state to the final state.
--
-- @@
-- | Our Definition | AWS             | DigitalOcean | Hetzner        |
-- |------------|------------ |----------|------------|
-- | "creating"     |                 |              | "initializing" |
-- | "creating"     | "pending"       | "new"        | "starting"     |
-- | "running"      | "running"       | "active"     | "running"      |
-- | "stopping"     | "stopping"      |              | "stopping"     |
-- | "stopped"      | "stopped"       | "off"        | "off"          |
-- | "terminating"  | "shutting-down" |              | "deleting"     |
-- | "rebuilding"   |                 |              | "rebuilding"   |
-- | "migrating"    |                 |              | "migrating"    |
-- | "terminated"   | "terminated"    |              |                |
-- | "archived"     |                 | "archive"    |                |
-- | "unknown"      |                 |              | "unknown"      |
-- @@
data State
  = StateCreating
  | StateRunning
  | StateStopping
  | StateStopped
  | StateTerminating
  | StateRebuilding
  | StateMigrating
  | StateTerminated
  | StateArchived
  | StateUnknown
  deriving (Bounded, Enum, Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec State)


-- | 'ADC.HasCodec' instance for 'State'.
instance ADC.HasCodec State where
  codec =
    _codec ADC.<?> "State"
    where
      _codec = ADC.stringConstCodec (NE.fromList (fmap (\x -> (x, stateCode x)) [minBound .. maxBound]))


-- | Returns a code for a given 'State'.
stateCode :: State -> T.Text
stateCode StateCreating = "creating"
stateCode StateRunning = "running"
stateCode StateStopping = "stopping"
stateCode StateStopped = "stopped"
stateCode StateTerminating = "terminating"
stateCode StateRebuilding = "rebuilding"
stateCode StateMigrating = "migrating"
stateCode StateTerminated = "terminated"
stateCode StateArchived = "archived"
stateCode StateUnknown = "unknown"


-- | Server.
data Server = Server
  { _serverId :: !T.Text
  , _serverName :: !(Maybe T.Text)
  , _serverCpu :: !(Maybe Int16)
  , _serverRam :: !(Maybe Int32)
  , _serverDisk :: !(Maybe Int32)
  , _serverState :: !State
  , _serverCreatedAt :: !(Maybe Time.UTCTime)
  , _serverProvider :: !Provider
  , _serverRegion :: !T.Text
  , _serverType :: !(Maybe T.Text)
  , _serverIpInfo :: !ServerIpInfo
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec Server)


instance ADC.HasCodec Server where
  codec =
    _codec ADC.<?> "Server"
    where
      _codec =
        ADC.object "Server" $
          Server
            <$> ADC.requiredField "id" "Server ID." ADC..= _serverId
            <*> ADC.optionalField "name" "Server name." ADC..= _serverName
            <*> ADC.requiredField "cpu" "Number of (v)CPUs." ADC..= _serverCpu
            <*> ADC.requiredField "ram" "Amount of RAM in MB." ADC..= _serverRam
            <*> ADC.requiredField "disk" "Amount of disk space in GB." ADC..= _serverDisk
            <*> ADC.requiredField "state" "Server state." ADC..= _serverState
            <*> ADC.requiredField "created_at" "Creation timestamp." ADC..= _serverCreatedAt
            <*> ADC.requiredField "provider" "Cloud provider." ADC..= _serverProvider
            <*> ADC.requiredField "region" "Region." ADC..= _serverRegion
            <*> ADC.optionalField "type" "Server type." ADC..= _serverType
            <*> ADC.requiredField "ip_info" "Server IP addresses information." ADC..= _serverIpInfo


-- | Server IP addresses information.
data ServerIpInfo = ServerIpInfo
  { _serverIpInfoStaticIpv4 :: ![Z.Net.IPv4]
  , _serverIpInfoStaticIpv6 :: ![Z.Net.IPv6]
  , _serverIpInfoPublicIpv4 :: ![Z.Net.IPv4]
  , _serverIpInfoPublicIpv6 :: ![Z.Net.IPv6]
  , _serverIpInfoPrivateIpv4 :: ![Z.Net.IPv4]
  , _serverIpInfoPrivateIpv6 :: ![Z.Net.IPv6]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ServerIpInfo)


instance ADC.HasCodec ServerIpInfo where
  codec =
    _codec ADC.<?> "Server IP Addresses Information"
    where
      _codec =
        ADC.object "ServerIpInfo" $
          ServerIpInfo
            <$> ADC.requiredField "static_ipv4" "Static IPv4 addresses." ADC..= _serverIpInfoStaticIpv4
            <*> ADC.requiredField "static_ipv6" "Static IPv6 addresses." ADC..= _serverIpInfoStaticIpv6
            <*> ADC.requiredField "public_ipv4" "Public IPv4 addresses." ADC..= _serverIpInfoPublicIpv4
            <*> ADC.requiredField "public_ipv6" "Public IPv6 addresses." ADC..= _serverIpInfoPublicIpv6
            <*> ADC.requiredField "private_ipv4" "Private IPv4 addresses." ADC..= _serverIpInfoPrivateIpv4
            <*> ADC.requiredField "private_ipv6" "Private IPv6 addresses." ADC..= _serverIpInfoPrivateIpv6


data ObjectBucket = ObjectBucket
  { _objectBucketName :: !T.Text
  , _objectBucketProvider :: !Provider
  , _objectBucketCreatedAt :: !(Maybe Time.UTCTime)
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ObjectBucket)


instance ADC.HasCodec ObjectBucket where
  codec =
    _codec ADC.<?> "Object Bucket"
    where
      _codec =
        ADC.object "ObjectBucket" $
          ObjectBucket
            <$> ADC.requiredField "name" "Bucket name." ADC..= _objectBucketName
            <*> ADC.requiredField "provider" "Cloud provider." ADC..= _objectBucketProvider
            <*> ADC.optionalField "created_at" "Creation timestamp." ADC..= _objectBucketCreatedAt
