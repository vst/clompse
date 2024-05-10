{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides functions to query remote AWS API and
-- convert responses to Clompse types.
module Clompse.Providers.Aws.ApiAws where

import qualified Amazonka as Aws
import qualified Amazonka.Data as Aws.Data
import qualified Amazonka.Data.Time as Aws.Data.Time
import qualified Amazonka.EC2 as Aws.Ec2
import qualified Amazonka.EC2.Lens as Aws.Ec2.Lens
import qualified Amazonka.EC2.Types as Aws.Ec2.Types
import qualified Amazonka.EC2.Types as Aws.Ec2.Types.InstanceTypeInfo
import qualified Amazonka.S3 as Aws.S3
import qualified Amazonka.S3.Lens as Aws.S3.Lens
import Clompse.Providers.Aws.Connection (AwsConnection, _envFromConnection)
import Clompse.Providers.Aws.Error (AwsError (..))
import qualified Clompse.Types as Types
import Conduit ((.|))
import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Lens as L
import Control.Monad (join)
import Control.Monad.Except (MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import Data.Maybe (catMaybes, fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Zamazingo.Net as Z.Net


-- * Operations


listServersEc2
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Types.Server]
listServersEc2 cfg = do
  instances <- awsEc2ListAllInstancesWithSecurityGroups cfg
  pure (fmap ec2InstanceToServer instances)


listBucketsS3
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(T.Text, Time.UTCTime)]
listBucketsS3 =
  awsListAllS3Buckets


-- * Data Definitions


-- * Helpers


-- ** Regions


awsEc2ListAllRegions
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Aws.Region]
awsEc2ListAllRegions cfg = do
  infos <- awsEc2ListAllRegionInfos cfg
  mapM _ec2RegionInfoToRegion infos


awsEc2ListAllRegionInfos
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Aws.Ec2.RegionInfo]
awsEc2ListAllRegionInfos cfg = do
  env <- _envFromConnection cfg
  (Aws.Ec2.DescribeRegionsResponse' ma _) <- liftIO (Aws.runResourceT (Aws.send env Aws.Ec2.newDescribeRegions))
  pure $ fromMaybe [] ma


_ec2RegionInfoToRegion
  :: MonadError AwsError m
  => Aws.Ec2.RegionInfo
  -> m Aws.Region
_ec2RegionInfoToRegion i =
  case i L.^. Aws.Ec2.Lens.regionInfo_regionName of
    Nothing -> throwError (AwsErrorLogical "No region name given")
    Just sv -> case Aws.Data.fromText sv of
      Left _ -> throwError (AwsErrorParsing "Can not parse region" sv)
      Right r -> pure r


-- ** Instances


type Ec2InstanceList = [Ec2InstanceListItem]


type Ec2InstanceListItem = (Aws.Region, Aws.Ec2.Instance, Maybe Int, Maybe Integer, Maybe Integer)


awsEc2ListAllInstances
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m Ec2InstanceList
awsEc2ListAllInstances cfg = do
  regions <- awsEc2ListAllRegions cfg
  res <- liftIO . Async.withTaskGroup 4 $ \tg -> Async.mapTasks tg (fmap (runExceptT . awsEc2ListAllInstancesForRegion cfg) regions)
  case concat <$> sequence res of
    Left e -> throwError e
    Right x -> pure x


awsEc2ListAllInstancesForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m Ec2InstanceList
awsEc2ListAllInstancesForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env Aws.Ec2.newDescribeInstances
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.describeInstancesResponse_reservations . L._Just)
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.reservation_instances . L._Just)
          .| CL.consume
  resIs <- liftIO . Aws.runResourceT . C.runConduit $ prog
  let instanceTypes = fmap (L.view Aws.Ec2.Lens.instance_instanceType) resIs
  resTs <- awsEc2InstanceTypeHw cfg reg instanceTypes
  pure $ fmap (mkItem resTs) resIs
  where
    mkItem resTs i = do
      let it = i L.^. Aws.Ec2.Lens.instance_instanceType
      case HM.lookup it resTs of
        Nothing -> (reg, i, Nothing, Nothing, Nothing)
        Just (cpu, ram, disk) -> (reg, i, cpu, ram, disk)


awsEc2InstanceTypeHw
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> [Aws.Ec2.InstanceType]
  -> m (HM.HashMap Aws.Ec2.InstanceType (Maybe Int, Maybe Integer, Maybe Integer))
awsEc2InstanceTypeHw cfg reg its = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env (Aws.Ec2.newDescribeInstanceTypes L.& Aws.Ec2.Lens.describeInstanceTypes_instanceTypes L.?~ its)
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.describeInstanceTypesResponse_instanceTypes . L._Just)
          .| CL.consume
  resTs <- liftIO . Aws.runResourceT . C.runConduit $ prog
  pure $ HM.fromList (mapMaybe mkRes resTs)
  where
    mkRes :: Aws.Ec2.InstanceTypeInfo -> Maybe (Aws.Ec2.InstanceType, (Maybe Int, Maybe Integer, Maybe Integer))
    mkRes i@Aws.Ec2.Types.InstanceTypeInfo.InstanceTypeInfo' {..} =
      case instanceType of
        Nothing -> Nothing
        Just it ->
          let cpu = join $ i L.^? Aws.Ec2.Lens.instanceTypeInfo_vCpuInfo L.^? L._Just . L._Just . Aws.Ec2.Lens.vCpuInfo_defaultVCpus
              ram = join $ i L.^? Aws.Ec2.Lens.instanceTypeInfo_memoryInfo L.^? L._Just . L._Just . Aws.Ec2.Lens.memoryInfo_sizeInMiB
              disk = join $ i L.^? Aws.Ec2.Lens.instanceTypeInfo_instanceStorageInfo L.^? L._Just . L._Just . Aws.Ec2.Lens.instanceStorageInfo_totalSizeInGB
           in Just (it, (cpu, ram, disk))


-- *** Security Groups


awsEc2ListAllSecurityGroups
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Aws.Ec2.SecurityGroup]
awsEc2ListAllSecurityGroups cfg = do
  regions <- awsEc2ListAllRegions cfg
  concat <$> mapM (awsEc2ListAllSecurityGroupsForRegion cfg) regions


awsEc2ListAllSecurityGroupsForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m [Aws.Ec2.SecurityGroup]
awsEc2ListAllSecurityGroupsForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env Aws.Ec2.newDescribeSecurityGroups
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.describeSecurityGroupsResponse_securityGroups . L._Just)
          .| CL.consume
  liftIO . Aws.runResourceT . C.runConduit $ prog


-- -- *** Instances with Security Groups

awsEc2ListAllInstancesWithSecurityGroups
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(Aws.Region, Aws.Ec2.Instance, Maybe Int, Maybe Integer, Maybe Integer, [Aws.Ec2.SecurityGroup])]
awsEc2ListAllInstancesWithSecurityGroups cfg = do
  instancesWithRegions <- awsEc2ListAllInstances cfg
  securityGroups <- awsEc2ListAllSecurityGroups cfg
  pure (fmap (\(r, i, m1, m2, m3) -> (r, i, m1, m2, m3, findSecurityGroups securityGroups i)) instancesWithRegions)
  where
    findSecurityGroups sgs i =
      let sids = catMaybes $ foldMap (fmap (L.^. Aws.Ec2.Lens.groupIdentifier_groupId)) (i L.^. Aws.Ec2.Lens.instance_securityGroups)
       in concatMap (\gi -> filter (\sg -> sg L.^. Aws.Ec2.Lens.securityGroup_groupId == gi) sgs) sids


-- ** S3 Buckets


awsListAllS3Buckets
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(T.Text, Time.UTCTime)]
awsListAllS3Buckets cfg = do
  env <- _envFromConnection cfg
  let prog = Aws.send env Aws.S3.newListBuckets
  resIs <- liftIO . fmap (fromMaybe [] . L.view Aws.S3.Lens.listBucketsResponse_buckets) . Aws.runResourceT $ prog
  pure $ fmap mkTuple resIs
  where
    mkTuple b =
      let name = b L.^. Aws.S3.Lens.bucket_name . Aws.S3._BucketName
          time = b L.^. Aws.S3.Lens.bucket_creationDate
       in (name, time)


-- ** Converters


ec2InstanceToServer :: (Aws.Region, Aws.Ec2.Instance, Maybe Int, Maybe Integer, Maybe Integer, [Aws.Ec2.SecurityGroup]) -> Types.Server
ec2InstanceToServer (region, i@Aws.Ec2.Instance' {..}, mCpu, mRam, mDisks, _sgs) =
  Types.Server
    { Types._serverId = instanceId
    , Types._serverName = awsEc2InstanceName i
    , Types._serverCpu = fromIntegral <$> mCpu
    , Types._serverRam = fromIntegral <$> mRam
    , Types._serverDisk = fromIntegral <$> mDisks
    , Types._serverState = ec2InstanceToServerState state
    , Types._serverCreatedAt = Just (Aws.Data.Time.fromTime launchTime)
    , Types._serverProvider = Types.ProviderAws
    , Types._serverRegion = Aws.fromRegion region
    , Types._serverType = Just (Aws.Ec2.fromInstanceType instanceType)
    , Types._serverIpInfo = ec2InstanceToServerIpInfo i
    , Types._serverFirewalls = fmap toFirewall _sgs
    }


toFirewall :: Aws.Ec2.SecurityGroup -> Types.Firewall
toFirewall sgs =
  let fid = sgs L.^. Aws.Ec2.Lens.securityGroup_groupId
      name = sgs L.^. Aws.Ec2.Lens.securityGroup_groupName
      inbound = fromMaybe [] $ sgs L.^. Aws.Ec2.Lens.securityGroup_ipPermissions
      outbound = fromMaybe [] $ sgs L.^. Aws.Ec2.Lens.securityGroup_ipPermissionsEgress
   in Types.Firewall
        { _firewallId = fid
        , _firewallName = Just name
        , _firewallRulesInbound = fmap toFirewallRule inbound
        , _firewallRulesOutbound = fmap toFirewallRule outbound
        , _firewallCreatedAt = Nothing
        }


toFirewallRule :: Aws.Ec2.IpPermission -> Types.FirewallRule
toFirewallRule ip =
  let proto = ip L.^. Aws.Ec2.Lens.ipPermission_ipProtocol
      fromPort = fromIntegral . fromMaybe 0 $ ip L.^. Aws.Ec2.Lens.ipPermission_fromPort
      toPort = fromIntegral . fromMaybe 0 $ ip L.^. Aws.Ec2.Lens.ipPermission_toPort
      ips = fromMaybe [] $ ip L.^. Aws.Ec2.Lens.ipPermission_ipRanges
   in Types.FirewallRule
        { _firewallRuleProtocol = proto
        , _firewallRulePorts = [Types.FirewallRulePorts {_firewallRulePortsFrom = fromPort, _firewallRulePortsTo = toPort}]
        , _firewallRuleEntities = fmap (L.^. Aws.Ec2.Lens.ipRange_cidrIp) ips
        }


ec2InstanceToServerState :: Aws.Ec2.Types.InstanceState -> Types.State
ec2InstanceToServerState Aws.Ec2.Types.InstanceState' {..} =
  case name of
    Aws.Ec2.Types.InstanceStateName_Pending -> Types.StateCreating
    Aws.Ec2.Types.InstanceStateName_Running -> Types.StateRunning
    Aws.Ec2.Types.InstanceStateName_Stopping -> Types.StateStopping
    Aws.Ec2.Types.InstanceStateName_Stopped -> Types.StateStopped
    Aws.Ec2.Types.InstanceStateName_Shutting_down -> Types.StateStopping
    Aws.Ec2.Types.InstanceStateName_Terminated -> Types.StateTerminating
    _ -> Types.StateUnknown


ec2InstanceToServerIpInfo :: Aws.Ec2.Instance -> Types.ServerIpInfo
ec2InstanceToServerIpInfo Aws.Ec2.Instance' {..} =
  Types.ServerIpInfo
    { _serverIpInfoStaticIpv4 = [] -- TODO: This is now reported below in public v4 field.
    , _serverIpInfoStaticIpv6 = [] -- TODO: Is there such thing for AWS EC2?
    , _serverIpInfoPublicIpv4 = maybeToList (Z.Net.ipv4FromText =<< publicIpAddress)
    , _serverIpInfoPublicIpv6 = maybeToList (Z.Net.ipv6FromText =<< ipv6Address)
    , _serverIpInfoPrivateIpv4 = maybeToList (Z.Net.ipv4FromText =<< privateIpAddress)
    , _serverIpInfoPrivateIpv6 = [] -- There is no such thing for AWS EC2.
    }


awsEc2InstanceName
  :: Aws.Ec2.Instance
  -> Maybe T.Text
awsEc2InstanceName i =
  let mTag = List.find (\t -> T.toLower (t L.^. Aws.Ec2.Lens.tag_key) == "name") =<< (i L.^. Aws.Ec2.Lens.instance_tags)
   in (L.^. Aws.Ec2.Lens.tag_value) <$> mTag
