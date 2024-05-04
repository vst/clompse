{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Providers.Do where

import qualified Amazonka as Aws
import qualified Amazonka.Auth as Aws.Auth
import qualified Amazonka.S3 as Aws.S3
import qualified Amazonka.S3.Lens as Aws.S3.Lens
import qualified Autodocodec as ADC
import qualified Clompse.Types as Types
import qualified Control.Lens as L
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Int (Int16, Int32, Int64)
import qualified Data.List as List
import Data.Maybe (fromMaybe)
import Data.Scientific (Scientific)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Time as Time
import GHC.Generics (Generic)
import System.Exit (ExitCode (..))
import qualified System.Process.Typed as TP
import qualified Zamazingo.Net as Z.Net
import qualified Zamazingo.Text as Z.Text


-- * Connection


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


-- * Error


data DoError
  = DoErrorConnection !T.Text
  | -- | Error encountered during reading DigitalOcean API (command,
    -- arguments, error message).
    DoErrorCommand !FilePath ![T.Text] !T.Text
  | -- | Error encountered during parsing DigitalOcean API result
    -- (error message, payload being parsed).
    DoErrorParsing !T.Text !BL.ByteString
  | DoErrorUnknown !T.Text
  deriving (Eq, Show)


-- * Data Definitions


-- ** Droplet


data DoDroplet = DoDroplet
  { _doDropletId :: !Int64
  , _doDropletName :: !T.Text
  , _doDropletMemory :: !Int32
  , _doDropletVcpus :: !Int16
  , _doDropletDisk :: !Int32
  , _doDropletRegion :: !DoRegion
  , _doDropletImage :: !DoImage
  , _doDropletSize :: !DoSize
  , _doDropletSizeSlug :: !T.Text
  , _doDropletFeatures :: ![T.Text]
  , _doDropletStatus :: !T.Text
  , _doDropletNetworks :: !DoNetworks
  , _doDropletCreatedAt :: !Time.UTCTime
  , _doDropletTags :: !(Maybe [T.Text])
  , _doDropletVolumeIds :: ![Int64]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoDroplet)


instance ADC.HasCodec DoDroplet where
  codec =
    _codec ADC.<?> "DigitalOcean Droplet"
    where
      _codec =
        ADC.object "DoDroplet" $
          DoDroplet
            <$> ADC.requiredField "id" "Droplet identifiers." ADC..= _doDropletId
            <*> ADC.requiredField "name" "Droplet name." ADC..= _doDropletName
            <*> ADC.requiredField "memory" "Droplet memory size." ADC..= _doDropletMemory
            <*> ADC.requiredField "vcpus" "Number of Droplet vCPUs." ADC..= _doDropletVcpus
            <*> ADC.requiredField "disk" "Droplet disk size." ADC..= _doDropletDisk
            <*> ADC.requiredField "region" "Droplet region." ADC..= _doDropletRegion
            <*> ADC.requiredField "image" "Droplet image." ADC..= _doDropletImage
            <*> ADC.requiredField "size" "Droplet size." ADC..= _doDropletSize
            <*> ADC.requiredField "size_slug" "Droplet size slug." ADC..= _doDropletSizeSlug
            <*> ADC.requiredField "features" "Droplet features." ADC..= _doDropletFeatures
            <*> ADC.requiredField "status" "Droplet status." ADC..= _doDropletStatus
            <*> ADC.requiredField "networks" "Droplet networks." ADC..= _doDropletNetworks
            <*> ADC.requiredField "created_at" "Droplet creation time." ADC..= _doDropletCreatedAt
            <*> ADC.optionalField "tags" "Droplet tags." ADC..= _doDropletTags
            <*> ADC.requiredField "volume_ids" "Droplet volume identifiers." ADC..= _doDropletVolumeIds


-- ** Region


data DoRegion = DoRegion
  { _doRegionSlug :: !T.Text
  , _doRegionName :: !T.Text
  , _doRegionSizes :: !(Maybe [T.Text])
  , _doRegionAvailable :: !(Maybe Bool)
  , _doRegionFeatures :: ![T.Text]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoRegion)


instance ADC.HasCodec DoRegion where
  codec =
    _codec ADC.<?> "DigitalOcean Region"
    where
      _codec =
        ADC.object "DoRegion" $
          DoRegion
            <$> ADC.requiredField "slug" "Region slug." ADC..= _doRegionSlug
            <*> ADC.requiredField "name" "Region name." ADC..= _doRegionName
            <*> ADC.optionalField "sizes" "Region sizes." ADC..= _doRegionSizes
            <*> ADC.optionalField "available" "Region availability." ADC..= _doRegionAvailable
            <*> ADC.requiredField "features" "Region features." ADC..= _doRegionFeatures


-- ** Image


data DoImage = DoImage
  { _doImageId :: !Int64
  , _doImageName :: !T.Text
  , _doImageType :: !T.Text
  , _doImageDistribution :: !T.Text
  , _doImageMinDiskSize :: !Int32
  , _doImageSizeGigabytes :: !Scientific
  , _doImageCreatedAt :: !Time.UTCTime
  , _doImageDescription :: !(Maybe T.Text)
  , _doImageStatus :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoImage)


instance ADC.HasCodec DoImage where
  codec =
    _codec ADC.<?> "DigitalOcean Image"
    where
      _codec =
        ADC.object "DoImage" $
          DoImage
            <$> ADC.requiredField "id" "Image identifier." ADC..= _doImageId
            <*> ADC.requiredField "name" "Image name." ADC..= _doImageName
            <*> ADC.requiredField "type" "Image type." ADC..= _doImageType
            <*> ADC.requiredField "distribution" "Image distribution." ADC..= _doImageDistribution
            <*> ADC.requiredField "min_disk_size" "Minimum disk size." ADC..= _doImageMinDiskSize
            <*> ADC.requiredField "size_gigabytes" "Image size in gigabytes." ADC..= _doImageSizeGigabytes
            <*> ADC.requiredField "created_at" "Image creation time." ADC..= _doImageCreatedAt
            <*> ADC.optionalField "description" "Image description." ADC..= _doImageDescription
            <*> ADC.requiredField "status" "Image status." ADC..= _doImageStatus


-- ** Size


data DoSize = DoSize
  { _doSizeSlug :: !T.Text
  , _doSizeMemory :: !Int32
  , _doSizeVcpus :: !Int16
  , _doSizeDisk :: !Int32
  , _doSizePriceMonthly :: !Scientific
  , _doSizePriceHourly :: !Scientific
  , _doSizeRegions :: ![T.Text]
  , _doSizeAvailable :: !Bool
  , _doSizeTransfer :: !Scientific
  , _doSizeDescription :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoSize)


instance ADC.HasCodec DoSize where
  codec =
    _codec ADC.<?> "DigitalOcean Size"
    where
      _codec =
        ADC.object "DoSize" $
          DoSize
            <$> ADC.requiredField "slug" "Size slug." ADC..= _doSizeSlug
            <*> ADC.requiredField "memory" "Size memory." ADC..= _doSizeMemory
            <*> ADC.requiredField "vcpus" "Number of vCPUs." ADC..= _doSizeVcpus
            <*> ADC.requiredField "disk" "Size disk." ADC..= _doSizeDisk
            <*> ADC.requiredField "price_monthly" "Monthly price." ADC..= _doSizePriceMonthly
            <*> ADC.requiredField "price_hourly" "Hourly price." ADC..= _doSizePriceHourly
            <*> ADC.requiredField "regions" "Size regions." ADC..= _doSizeRegions
            <*> ADC.requiredField "available" "Size availability." ADC..= _doSizeAvailable
            <*> ADC.requiredField "transfer" "Size transfer." ADC..= _doSizeTransfer
            <*> ADC.requiredField "description" "Size description." ADC..= _doSizeDescription


-- ** Networking


-- *** Networks


data DoNetworks = DoNetworks
  { _doNetworksV4 :: !(Maybe [DoNetworkV4])
  , _doNetworksV6 :: !(Maybe [DoNetworkV6])
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoNetworks)


instance ADC.HasCodec DoNetworks where
  codec =
    _codec ADC.<?> "DigitalOcean Networks"
    where
      _codec =
        ADC.object "DoNetworks" $
          DoNetworks
            <$> ADC.optionalField "v4" "IPv4 networks." ADC..= _doNetworksV4
            <*> ADC.optionalField "v6" "IPv6 networks." ADC..= _doNetworksV6


-- **** IPv4 Network


data DoNetworkV4 = DoNetworkV4
  { _doNetworkV4IpAddress :: !Z.Net.IPv4
  , _doNetworkV4Netmask :: !Z.Net.IPv4
  , _doNetworkV4Gateway :: !Z.Net.IPv4
  , _doNetworkV4Type :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoNetworkV4)


instance ADC.HasCodec DoNetworkV4 where
  codec =
    _codec ADC.<?> "DigitalOcean IPv4 Network"
    where
      _codec =
        ADC.object "DoNetworkV4" $
          DoNetworkV4
            <$> ADC.requiredField "ip_address" "IPv4 address." ADC..= _doNetworkV4IpAddress
            <*> ADC.requiredField "netmask" "IPv4 netmask." ADC..= _doNetworkV4Netmask
            <*> ADC.requiredField "gateway" "IPv4 gateway." ADC..= _doNetworkV4Gateway
            <*> ADC.requiredField "type" "IPv4 type." ADC..= _doNetworkV4Type


-- **** IPv6 Network


data DoNetworkV6 = DoNetworkV6
  { _doNetworkV6IpAddress :: !Z.Net.IPv6
  , _doNetworkV6Netmask :: !Int64
  , _doNetworkV6Gateway :: !Z.Net.IPv6
  , _doNetworkV6Type :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoNetworkV6)


instance ADC.HasCodec DoNetworkV6 where
  codec =
    _codec ADC.<?> "DigitalOcean IPv6 Network"
    where
      _codec =
        ADC.object "DoNetworkV6" $
          DoNetworkV6
            <$> ADC.requiredField "ip_address" "IPv6 address." ADC..= _doNetworkV6IpAddress
            <*> ADC.requiredField "netmask" "IPv6 netmask." ADC..= _doNetworkV6Netmask
            <*> ADC.requiredField "gateway" "IPv6 gateway." ADC..= _doNetworkV6Gateway
            <*> ADC.requiredField "type" "IPv6 type." ADC..= _doNetworkV6Type


-- *** Firewall


data DoFirewall = DoFirewall
  { _doFirewallId :: !T.Text -- TODO: Change to UUID
  , _doFirewallName :: !T.Text
  , _doFirewallStatus :: !T.Text
  , _doFirewallInboundRules :: ![DoFirewallInboundRule]
  , _doFirewallOutboundRules :: ![DoFirewallOutboundRule]
  , _doFirewallDropletIds :: ![Int64]
  , _doFirewallTags :: ![T.Text]
  , _doFirewallCreatedAt :: !Time.UTCTime
  , _doFirewallPendingChanges :: !Aeson.Value
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoFirewall)


instance ADC.HasCodec DoFirewall where
  codec =
    _codec ADC.<?> "DigitalOcean Firewall"
    where
      _codec =
        ADC.object "DoFirewall" $
          DoFirewall
            <$> ADC.requiredField "id" "Firewall identifier." ADC..= _doFirewallId
            <*> ADC.requiredField "name" "Firewall name." ADC..= _doFirewallName
            <*> ADC.requiredField "status" "Firewall status." ADC..= _doFirewallStatus
            <*> ADC.requiredField "inbound_rules" "Firewall inbound rules." ADC..= _doFirewallInboundRules
            <*> ADC.requiredField "outbound_rules" "Firewall outbound rules." ADC..= _doFirewallOutboundRules
            <*> ADC.requiredField "droplet_ids" "Firewall droplet identifiers." ADC..= _doFirewallDropletIds
            <*> ADC.requiredField "tags" "Firewall tags." ADC..= _doFirewallTags
            <*> ADC.requiredField "created_at" "Firewall creation time." ADC..= _doFirewallCreatedAt
            <*> ADC.requiredField "pending_changes" "Firewall pending changes." ADC..= _doFirewallPendingChanges


-- **** Inbound Rule


data DoFirewallInboundRule = DoFirewallInboundRule
  { _doFirewallInboundRuleProtocol :: !T.Text
  , _doFirewallInboundRulePorts :: !T.Text
  , _doFirewallInboundRuleSources :: !Aeson.Value
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoFirewallInboundRule)


instance ADC.HasCodec DoFirewallInboundRule where
  codec =
    _codec ADC.<?> "DigitalOcean Firewall Inbound Rule"
    where
      _codec =
        ADC.object "DoFirewallInboundRule" $
          DoFirewallInboundRule
            <$> ADC.requiredField "protocol" "Firewall inbound rule protocol." ADC..= _doFirewallInboundRuleProtocol
            <*> ADC.requiredField "ports" "Firewall inbound rule ports." ADC..= _doFirewallInboundRulePorts
            <*> ADC.requiredField "sources" "Firewall inbound rule sources." ADC..= _doFirewallInboundRuleSources


-- **** Outbound Rule


data DoFirewallOutboundRule = DoFirewallOutboundRule
  { _doFirewallOutboundRuleProtocol :: !T.Text
  , _doFirewallOutboundRulePorts :: !T.Text
  , _doFirewallOutboundRuleDestinations :: !Aeson.Value
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DoFirewallOutboundRule)


instance ADC.HasCodec DoFirewallOutboundRule where
  codec =
    _codec ADC.<?> "DigitalOcean Firewall Outbound Rule"
    where
      _codec =
        ADC.object "DoFirewallOutboundRule" $
          DoFirewallOutboundRule
            <$> ADC.requiredField "protocol" "Firewall outbound rule protocol." ADC..= _doFirewallOutboundRuleProtocol
            <*> ADC.requiredField "ports" "Firewall outbound rule ports." ADC..= _doFirewallOutboundRulePorts
            <*> ADC.requiredField "destinations" "Firewall outbound rule destinations." ADC..= _doFirewallOutboundRuleDestinations


-- * Operations


-- ** List Droplets


doListDroplets
  :: MonadIO m
  => MonadError DoError m
  => DoConnection
  -> m [DoDroplet]
doListDroplets conn =
  doctl conn ["compute", "droplet", "list"]


-- ** List Firewalls


doListFirewalls
  :: MonadIO m
  => MonadError DoError m
  => DoConnection
  -> m [DoFirewall]
doListFirewalls conn =
  doctl conn ["compute", "firewall", "list"]


-- ** List Regions


doListRegions
  :: MonadIO m
  => MonadError DoError m
  => DoConnection
  -> m [DoRegion]
doListRegions conn =
  doctl conn ["compute", "region", "list"]


-- ** List Spaces Buckets


doListSpacesBuckets
  :: MonadIO m
  => MonadError DoError m
  => DoConnection
  -> m [(T.Text, Time.UTCTime)]
doListSpacesBuckets conn = do
  let accessKeyId = _doConnectionSpacesAccessKeyId conn
  let secretAccessKey = _doConnectionSpacesSecretAccessKey conn
  case (accessKeyId, secretAccessKey) of
    (Just sa, Just ss) -> do
      regions <- filter isAvail <$> doListRegions conn
      List.concat <$> traverse (doListSpacesBucketsForRegion conn sa ss) regions
    _ -> pure []
  where
    avail = ["nyc3", "ams3", "sfo2", "sfo3", "sgp1", "fra1", "blr1", "syd1"]
    isAvail = (`List.elem` avail) . _doRegionSlug


doListSpacesBucketsForRegion
  :: MonadIO m
  => MonadError DoError m
  => DoConnection
  -> T.Text
  -> T.Text
  -> DoRegion
  -> m [(T.Text, Time.UTCTime)]
doListSpacesBucketsForRegion _conn accessKeyId secretAccessKey region = do
  env <- _envFromConnection accessKeyId secretAccessKey (_doRegionSlug region)
  let prog = Aws.send env Aws.S3.newListBuckets
  resIs <- liftIO . fmap (fromMaybe [] . L.view Aws.S3.Lens.listBucketsResponse_buckets) . Aws.runResourceT $ prog
  pure $ fmap mkTuple resIs
  where
    mkTuple b =
      let name = b L.^. Aws.S3.Lens.bucket_name . Aws.S3._BucketName
          time = b L.^. Aws.S3.Lens.bucket_creationDate
       in (name, time)


_envFromConnection
  :: MonadIO m
  => T.Text
  -> T.Text
  -> T.Text
  -> m Aws.Env
_envFromConnection accessKeyId secretAccessKey region =
  (\x -> x {Aws.overrides = service}) <$> Aws.newEnv (pure . Aws.Auth.fromKeys accessKeyId' secretAccessKey')
  where
    service =
      const $
        Aws.S3.defaultService
          { Aws.endpoint =
              const $
                Aws.Endpoint
                  { host = TE.encodeUtf8 (region <> ".digitaloceanspaces.com")
                  , basePath = mempty
                  , secure = True
                  , port = 443
                  , scope = TE.encodeUtf8 region
                  }
          }
    accessKeyId' = Aws.AccessKey (TE.encodeUtf8 accessKeyId)
    secretAccessKey' = Aws.SecretKey (TE.encodeUtf8 secretAccessKey)


-- * Helpers


toServer :: DoDroplet -> Types.Server
toServer droplet@DoDroplet {..} =
  Types.Server
    { _serverId = Z.Text.tshow _doDropletId
    , _serverName = Just _doDropletName
    , _serverCpu = Just _doDropletVcpus
    , _serverRam = Just _doDropletMemory
    , _serverDisk = Just _doDropletDisk
    , _serverState = toServerState _doDropletStatus
    , _serverCreatedAt = Just _doDropletCreatedAt
    , _serverProvider = Types.ProviderDo
    , _serverRegion = _doRegionSlug _doDropletRegion
    , _serverType = Just _doDropletSizeSlug
    , _serverIpInfo = toServerIpInfo droplet
    }


toServerIpInfo :: DoDroplet -> Types.ServerIpInfo
toServerIpInfo DoDroplet {..} =
  let nets4 = fromMaybe [] (_doNetworksV4 _doDropletNetworks)
      nets6 = fromMaybe [] (_doNetworksV6 _doDropletNetworks)
      ipv4s = fmap ((,) <$> _doNetworkV4IpAddress <*> _doNetworkV4Type) nets4
      ipv6s = fmap ((,) <$> _doNetworkV6IpAddress <*> _doNetworkV6Type) nets6
   in Types.ServerIpInfo
        { _serverIpInfoStaticIpv4 = [] -- TODO: For now, reserved IP is seen in public IP section below.
        , _serverIpInfoStaticIpv6 = [] -- No such thing for DO.
        , _serverIpInfoPublicIpv4 = List.nub [ip | (ip, "public") <- ipv4s]
        , _serverIpInfoPublicIpv6 = List.nub [ip | (ip, "public") <- ipv6s]
        , _serverIpInfoPrivateIpv4 = List.nub [ip | (ip, "private") <- ipv4s]
        , _serverIpInfoPrivateIpv6 = List.nub [ip | (ip, "private") <- ipv6s]
        }


toServerState :: T.Text -> Types.State
toServerState "new" = Types.StateCreating
toServerState "active" = Types.StateRunning
toServerState "off" = Types.StateStopped
toServerState "archive" = Types.StateArchived
toServerState _ = Types.StateUnknown


doctl
  :: Aeson.FromJSON a
  => MonadIO m
  => MonadError DoError m
  => DoConnection
  -> [T.Text]
  -> m a
doctl DoConnection {..} args = do
  (ec, out, err) <- procRead
  case ec of
    ExitSuccess -> either (throwError . flip DoErrorParsing out . T.pack) pure (Aeson.eitherDecode out)
    ExitFailure _ -> throwError (DoErrorCommand procPath procArgs (TL.toStrict (TLE.decodeUtf8 err)))
  where
    procPath = "doctl"
    procArgs = ["--context", "default", "--access-token", _doConnectionToken, "--output", "json"] <> args
    procRead = TP.readProcess (TP.proc procPath (fmap T.unpack procArgs))
