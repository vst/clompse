{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Clompse.Providers.Aws.ApiLightsail where

import qualified Amazonka as Aws
import qualified Amazonka.Data as Aws.Data
import qualified Amazonka.Lightsail as Aws.Lightsail
import qualified Amazonka.Lightsail.Lens as Aws.Lightsail.Lens
import Clompse.Providers.Aws.ApiAws (awsEc2ListAllRegions)
import Clompse.Providers.Aws.Connection (AwsConnection, _envFromConnection)
import Clompse.Providers.Aws.Error (AwsError (..))
import qualified Clompse.Types as Types
import Conduit ((.|))
import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Lens as L
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Int (Int16)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Time as Time
import GHC.Float (double2Int)
import qualified Zamazingo.Net as Z.Net
import qualified Zamazingo.Text as Z.Text


-- * Operations


listServersLightsail
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Types.Server]
listServersLightsail cfg = do
  instances <- awsLightsailListAllInstances cfg
  pure $ uncurry lightsailInstanceToServer <$> instances


listBucketsLightsail
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(T.Text, Time.UTCTime)]
listBucketsLightsail =
  awsListAllLightsailBuckets


listDomainsLightsail
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Types.Domain]
listDomainsLightsail cfg = do
  env <- _envFromConnection cfg
  let prog = Aws.send env Aws.Lightsail.newGetDomains
  resIs <- liftIO . fmap (fromMaybe [] . L.view Aws.Lightsail.Lens.getDomainsResponse_domains) . Aws.runResourceT $ prog
  pure $ fmap mkTuple resIs
  where
    mkTuple b =
      let name = b L.^. Aws.Lightsail.Lens.domain_name
       in Types.Domain
            { Types._domainName = fromMaybe "<unknown-lightsail-domain>" name
            , Types._domainProvider = Types.ProviderAws
            }


listDnsRecordsLightsail
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Types.DnsRecord]
listDnsRecordsLightsail cfg = do
  env <- _envFromConnection cfg
  let prog = Aws.send env Aws.Lightsail.newGetDomains
  resIs <- liftIO . fmap (fromMaybe [] . L.view Aws.Lightsail.Lens.getDomainsResponse_domains) . Aws.runResourceT $ prog
  pure $ concatMap mkEntries resIs
  where
    mkEntries b =
      let name = fromMaybe "<unknown>" (b L.^. Aws.Lightsail.Lens.domain_name)
          entries = fromMaybe [] (b L.^. Aws.Lightsail.Lens.domain_domainEntries)
       in fmap (mkEntry name) entries
    mkEntry name e =
      let _dnsRecordId = e L.^. Aws.Lightsail.Lens.domainEntry_id
          _dnsRecordName = fromMaybe "<unknown" (e L.^. Aws.Lightsail.Lens.domainEntry_name)
          _dnsRecordType = fromMaybe "<unknown" (e L.^. Aws.Lightsail.Lens.domainEntry_type)
          _dnsRecordTtl = 60 -- Lightsail does not provide TTL management.
          _dnsRecordValue = fromMaybe "" (e L.^. Aws.Lightsail.Lens.domainEntry_target)
          _dnsRecordPriority = Nothing
          _dnsRecordPort = Nothing
          _dnsRecordWeight = Nothing
          _dnsRecordFlags = Nothing
       in Types.DnsRecord
            { _dnsRecordProvider = Types.ProviderAws
            , _dnsRecordDomain = name
            , ..
            }


-- let _dnsRecordId = b L.^. Aws.Route53.Lens.resourceRecordSet_setIdentifier
--     _dnsRecordName = b L.^. Aws.Route53.Lens.resourceRecordSet_name
--     _dnsRecordType = Aws.Route53.fromRRType (b L.^. Aws.Route53.Lens.resourceRecordSet_type)
--     _dnsRecordTtl = maybe 0 fromIntegral $ b L.^. Aws.Route53.Lens.resourceRecordSet_ttl
--     _dnsRecordValue = foldMap (T.intercalate " # " . fmap (L.view Aws.Route53.Lens.resourceRecord_value) . NE.toList) $ b L.^. Aws.Route53.Lens.resourceRecordSet_resourceRecords
--     _dnsRecordPriority = Nothing
--     _dnsRecordPort = Nothing
--     _dnsRecordWeight = fromIntegral <$> b L.^. Aws.Route53.Lens.resourceRecordSet_weight
--     _dnsRecordFlags = Nothing
--  in Types.DnsRecord
--       { _dnsRecordProvider = Types.ProviderAws
--       , _dnsRecordDomain = dmn
--       , ..
--       }

-- * Data Definitions


-- * Helpers


-- ** Regions


awsLightsailListAllRegions
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Aws.Region]
awsLightsailListAllRegions cfg = do
  infos <- awsLightsailListAllRegionInfos cfg
  lsRegions <- mapM _lightsailRegionInfoToRegion infos
  awRegions <- awsEc2ListAllRegions cfg
  pure $ filter (`elem` awRegions) lsRegions


awsLightsailListAllRegionInfos
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Aws.Lightsail.RegionInfo]
awsLightsailListAllRegionInfos cfg = do
  env <- _envFromConnection cfg
  (Aws.Lightsail.GetRegionsResponse' ma _) <- liftIO (Aws.runResourceT (Aws.send env Aws.Lightsail.newGetRegions))
  pure $ fromMaybe [] ma


_lightsailRegionInfoToRegion
  :: MonadError AwsError m
  => Aws.Lightsail.RegionInfo
  -> m Aws.Region
_lightsailRegionInfoToRegion i =
  case i L.^. Aws.Lightsail.Lens.regionInfo_name of
    Nothing -> throwError (AwsErrorLogical "No region name given")
    Just sv -> case Aws.Data.fromText (Aws.Lightsail.fromRegionName sv) of
      Left _ -> throwError (AwsErrorParsing "Can not parse region" (Z.Text.tshow sv))
      Right r -> pure r


-- ** Instances


awsLightsailListAllInstances
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(Aws.Region, Aws.Lightsail.Instance)]
awsLightsailListAllInstances cfg = do
  regions <- awsLightsailListAllRegions cfg
  res <- liftIO . Async.withTaskGroup 4 $ \tg -> Async.mapTasks tg (fmap (runExceptT . awsLightsailListAllInstancesForRegion cfg) regions)
  case concat <$> sequence res of
    Left e -> throwError e
    Right x -> pure x


awsLightsailListAllInstancesForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m [(Aws.Region, Aws.Lightsail.Instance)]
awsLightsailListAllInstancesForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env Aws.Lightsail.newGetInstances
          .| CL.concatMap (L.view $ Aws.Lightsail.Lens.getInstancesResponse_instances . L._Just)
          .| CL.consume
  fmap (fmap (reg,)) . liftIO . Aws.runResourceT . C.runConduit $ prog


-- ** Buckets


awsListAllLightsailBuckets
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(T.Text, Time.UTCTime)]
awsListAllLightsailBuckets cfg = do
  regions <- awsLightsailListAllRegions cfg
  res <- liftIO . Async.withTaskGroup 4 $ \tg -> Async.mapTasks tg (fmap (runExceptT . awsListAllLightsailBucketsForRegion cfg) regions)
  case concat <$> sequence res of
    Left e -> throwError e
    Right x -> pure x


awsListAllLightsailBucketsForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m [(T.Text, Time.UTCTime)]
awsListAllLightsailBucketsForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog = Aws.send env Aws.Lightsail.newGetBuckets
  resIs <- liftIO . Aws.runResourceT $ prog
  -- NOTE: Amazonka does not support pagination over Lightsail buckets.
  -- let prog =
  --       Aws.paginate env Aws.Lightsail.newGetBuckets
  --         .| CL.concatMap (L.view $ Aws.Lightsail.Lens.getBucketsResponse_buckets . L._Just)
  --         .| CL.consume
  -- resIs <- liftIO . Aws.runResourceT . C.runConduit $ prog
  let buckets = fromMaybe [] $ resIs L.^. Aws.Lightsail.Lens.getBucketsResponse_buckets
  pure $ mapMaybe mkTuple buckets
  where
    mkTuple b =
      let name = b L.^. Aws.Lightsail.Lens.bucket_name
          time = b L.^. Aws.Lightsail.Lens.bucket_createdAt
       in (,) <$> name <*> time


-- ** Converters


lightsailInstanceToServer :: Aws.Region -> Aws.Lightsail.Instance -> Types.Server
lightsailInstanceToServer region i =
  let arn' = fromMaybe "<unknown>" (i L.^. Aws.Lightsail.Lens.instance_arn)
      segments = T.split (== '/') arn'
      _serverId = case segments of
        [_, x] -> x
        _ -> arn'
      _serverName = i L.^. Aws.Lightsail.Lens.instance_name
      _serverCpu = fromIntegral <$> i L.^? Aws.Lightsail.Lens.instance_hardware . L._Just . Aws.Lightsail.Lens.instanceHardware_cpuCount . L._Just
      _serverRam = fromIntegral . double2Int <$> i L.^? Aws.Lightsail.Lens.instance_hardware . L._Just . Aws.Lightsail.Lens.instanceHardware_ramSizeInGb . L._Just
      _serverDisk = sum . fmap (\x -> maybe 0 fromIntegral (x L.^. Aws.Lightsail.Lens.disk_sizeInGb)) <$> i L.^. Aws.Lightsail.Lens.instance_hardware . L._Just . Aws.Lightsail.Lens.instanceHardware_disks
      _serverState = maybe Types.StateUnknown lightsailInstanceToServerState (i L.^. Aws.Lightsail.Lens.instance_state)
      _serverCreatedAt = i L.^. Aws.Lightsail.Lens.instance_createdAt
      _serverProvider = Types.ProviderAws
      _serverRegion = Aws.fromRegion region
      _serverType = i L.^. Aws.Lightsail.Lens.instance_bundleId
      _serverIpInfo = lightsailInstanceToServerIpInfo i
      _serverFirewalls = foldMap (fmap toFirewall) $ i L.^? Aws.Lightsail.Lens.instance_networking . L._Just . Aws.Lightsail.Lens.instanceNetworking_ports . L._Just
   in Types.Server {..}
  where
    _toInt16 :: Int -> Int16
    _toInt16 = fromIntegral


toFirewall :: Aws.Lightsail.InstancePortInfo -> Types.Firewall
toFirewall i =
  let _firewallId = "#N/A"
      _firewallName = Nothing
      _firewallCreatedAt = Nothing
      _isIn = case i L.^. Aws.Lightsail.Lens.instancePortInfo_accessDirection of
        Just Aws.Lightsail.AccessDirection_Inbound -> True
        Just Aws.Lightsail.AccessDirection_Outbound -> False
        _ -> True
      rule =
        Types.FirewallRule
          { _firewallRuleProtocol = maybe "#N/A" Aws.Data.toText (i L.^. Aws.Lightsail.Lens.instancePortInfo_protocol)
          , _firewallRulePorts =
              [ Types.FirewallRulePorts
                  { _firewallRulePortsFrom = maybe 0 fromIntegral (i L.^. Aws.Lightsail.Lens.instancePortInfo_fromPort)
                  , _firewallRulePortsTo = maybe 0 fromIntegral (i L.^. Aws.Lightsail.Lens.instancePortInfo_toPort)
                  }
              ]
          , _firewallRuleEntities = fromMaybe [] (i L.^. Aws.Lightsail.Lens.instancePortInfo_cidrs) <> fromMaybe [] (i L.^. Aws.Lightsail.Lens.instancePortInfo_ipv6Cidrs)
          }
      _firewallRulesInbound = ([rule | _isIn])
      _firewallRulesOutbound = ([rule | not _isIn])
   in Types.Firewall {..}


lightsailInstanceToServerState :: Aws.Lightsail.InstanceState -> Types.State
lightsailInstanceToServerState i =
  case i L.^. Aws.Lightsail.Lens.instanceState_name of
    Just "pending" -> Types.StateCreating
    Just "running" -> Types.StateRunning
    Just "stopping" -> Types.StateStopping
    Just "stopped" -> Types.StateStopped
    Just "shutting-down" -> Types.StateTerminating
    Just "terminated" -> Types.StateTerminated
    _ -> Types.StateUnknown


lightsailInstanceToServerIpInfo :: Aws.Lightsail.Instance -> Types.ServerIpInfo
lightsailInstanceToServerIpInfo i =
  let isStaticIp = i L.^. Aws.Lightsail.Lens.instance_isStaticIp
      publicIpAddress = i L.^. Aws.Lightsail.Lens.instance_publicIpAddress
      ipv6Addresses = i L.^. Aws.Lightsail.Lens.instance_ipv6Addresses
      privateIpAddress = i L.^. Aws.Lightsail.Lens.instance_privateIpAddress
      hasStatic = fromMaybe False isStaticIp
      static4 = if hasStatic then publicIpAddress else Nothing
      public4 = if hasStatic then Nothing else publicIpAddress
   in Types.ServerIpInfo
        { _serverIpInfoStaticIpv4 = maybeToList (Z.Net.ipv4FromText =<< static4)
        , _serverIpInfoStaticIpv6 = [] -- TODO: Is there such thing for AWS Lightsail?
        , _serverIpInfoPublicIpv4 = maybeToList (Z.Net.ipv4FromText =<< public4)
        , _serverIpInfoPublicIpv6 = mapMaybe Z.Net.ipv6FromText (fromMaybe [] ipv6Addresses)
        , _serverIpInfoPrivateIpv4 = maybeToList (Z.Net.ipv4FromText =<< privateIpAddress)
        , _serverIpInfoPrivateIpv6 = [] -- TODO: Is there such thing for AWS Lightsail?
        }
