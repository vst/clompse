{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Clompse.Providers.Aws where

import qualified Amazonka as Aws
import qualified Amazonka.Auth as Aws.Auth
import qualified Amazonka.Data as Aws.Data
import qualified Amazonka.Data.Time as Aws.Data.Time
import qualified Amazonka.EC2 as Aws.Ec2
import qualified Amazonka.EC2.DescribeInstanceTypes as Aws.Ec2.DescribeInstanceTypes
import qualified Amazonka.EC2.Lens as Aws.Ec2.Lens
import qualified Amazonka.EC2.Types as Aws.Ec2.Types
import qualified Amazonka.EC2.Types.InstanceTypeInfo as Aws.Ec2.Types.InstanceTypeInfo
import qualified Amazonka.Lightsail as Aws.Lightsail
import qualified Amazonka.Lightsail.Lens as Aws.Lightsail.Lens
import qualified Amazonka.Lightsail.Types as Aws.Lightsail.Types
import qualified Amazonka.Lightsail.Types.Disk as Aws.Lightsail.Types.Disk
import qualified Autodocodec as ADC
import qualified Clompse.Types as Types
import Conduit ((.|))
import qualified Control.Concurrent.Async.Pool as Async
import qualified Control.Lens as L
import Control.Monad (join)
import Control.Monad.Except (MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.HashMap.Strict as HM
import Data.Int (Int16, Int32)
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Float (double2Int)
import GHC.Generics (Generic)
import qualified Zamazingo.Net as Z.Net
import qualified Zamazingo.Text as Z.Text


-- * Connection


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


-- * Error


data AwsError
  = -- | Error encountered during parsing DigitalOcean API result
    -- (error message, payload being parsed).
    AwsErrorParsing T.Text T.Text
  | AwsErrorLogical T.Text
  | AwsErrorUnknown T.Text
  deriving (Eq, Show)


-- * Operations


-- ** AWS EC2


-- *** Regions


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
  case i.regionName of
    Nothing -> throwError (AwsErrorLogical "No region name given")
    Just sv -> case Aws.Data.fromText sv of
      Left _ -> throwError (AwsErrorParsing "Can not parse region" sv)
      Right r -> pure r


-- *** Instances


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
        Aws.paginate env (Aws.Ec2.newDescribeInstanceTypes L.& Aws.Ec2.DescribeInstanceTypes.describeInstanceTypes_instanceTypes L.?~ its)
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

-- awsEc2ListAllInstancesWithSecurityGroups
--   :: MonadIO m
--   => MonadError AwsError m
--   => AwsConnection
--   -> m [(Aws.Region, Aws.Ec2.Instance, [Aws.Ec2.SecurityGroup])]
-- awsEc2ListAllInstancesWithSecurityGroups cfg = do
--   instancesWithRegions <- awsEc2ListAllInstances cfg
--   securityGroups <- awsEc2ListAllSecurityGroups cfg
--   pure (fmap (\(r, i) -> (r, i, findSecurityGroups securityGroups i)) instancesWithRegions)
--   where
--     findSecurityGroups sgs i =
--       let sids = catMaybes $ foldMap (fmap (L.^. Aws.Ec2.Lens.groupIdentifier_groupId)) (i L.^. Aws.Ec2.Lens.instance_securityGroups)
--        in concatMap (\gi -> filter (\sg -> sg L.^. Aws.Ec2.Lens.securityGroup_groupId == gi) sgs) sids

-- ** AWS Lightsail


-- *** Regions


awsLightsailListAllRegions
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [Aws.Region]
awsLightsailListAllRegions cfg = do
  infos <- awsLightsailListAllRegionInfos cfg
  mapM _lightsailRegionInfoToRegion infos


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


-- *** Instances


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


-- * Helpers


-- ** EC2


ec2InstanceToServer :: (Aws.Region, Aws.Ec2.Instance, Maybe Int, Maybe Integer, Maybe Integer) -> Types.Server
ec2InstanceToServer (region, i@Aws.Ec2.Instance' {..}, mCpu, mRam, mDisks) =
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
  let mTag = L.find (\t -> T.toLower (t L.^. Aws.Ec2.Lens.tag_key) == "name") =<< (i L.^. Aws.Ec2.Lens.instance_tags)
   in (L.^. Aws.Ec2.Lens.tag_value) <$> mTag


-- ** Lightsail


lightsailInstanceToServer :: Aws.Region -> Aws.Lightsail.Instance -> Types.Server
lightsailInstanceToServer region i@Aws.Lightsail.Types.Instance' {..} =
  Types.Server
    { Types._serverId = fromMaybe "<unknown>" arn
    , Types._serverName = name
    , Types._serverCpu = lightsailInstanceCpu =<< hardware
    , Types._serverRam = lightsailInstanceRam =<< hardware
    , Types._serverDisk = lightsailInstanceDisk =<< hardware
    , Types._serverState = maybe Types.StateUnknown lightsailInstanceToServerState state
    , Types._serverCreatedAt = Aws.Data.Time.fromTime <$> createdAt
    , Types._serverProvider = Types.ProviderAws
    , Types._serverRegion = Aws.fromRegion region
    , Types._serverType = bundleId
    , Types._serverIpInfo = lightsailInstanceToServerIpInfo i
    }


lightsailInstanceCpu :: Aws.Lightsail.Types.InstanceHardware -> Maybe Int16
lightsailInstanceCpu Aws.Lightsail.Types.InstanceHardware' {..} =
  fromIntegral <$> cpuCount


lightsailInstanceRam :: Aws.Lightsail.Types.InstanceHardware -> Maybe Int32
lightsailInstanceRam Aws.Lightsail.Types.InstanceHardware' {..} =
  fromIntegral . double2Int . (1024 *) <$> ramSizeInGb


lightsailInstanceDisk :: Aws.Lightsail.Types.InstanceHardware -> Maybe Int32
lightsailInstanceDisk Aws.Lightsail.Types.InstanceHardware' {..} =
  sum . fmap (maybe 0 fromIntegral . Aws.Lightsail.Types.Disk.sizeInGb) <$> disks


lightsailInstanceToServerState :: Aws.Lightsail.Types.InstanceState -> Types.State
lightsailInstanceToServerState Aws.Lightsail.Types.InstanceState' {..} =
  case name of
    Just "pending" -> Types.StateCreating
    Just "running" -> Types.StateRunning
    Just "stopping" -> Types.StateStopping
    Just "stopped" -> Types.StateStopped
    Just "shutting-down" -> Types.StateTerminating
    Just "terminated" -> Types.StateTerminated
    _ -> Types.StateUnknown


lightsailInstanceToServerIpInfo :: Aws.Lightsail.Instance -> Types.ServerIpInfo
lightsailInstanceToServerIpInfo Aws.Lightsail.Instance' {..} =
  let hasStatic = fromMaybe False isStaticIp
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


-- ** Others


_envFromConnection
  :: MonadIO m
  => AwsConnection
  -> m Aws.Env
_envFromConnection AwsConnection {..} =
  Aws.newEnv (pure . Aws.Auth.fromKeys accessKeyId secretAccessKey)
  where
    accessKeyId = Aws.AccessKey (TE.encodeUtf8 _awsConnectionAccessKeyId)
    secretAccessKey = Aws.SecretKey (TE.encodeUtf8 _awsConnectionSecretAccessKey)
