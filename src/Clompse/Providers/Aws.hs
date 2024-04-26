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
import qualified Amazonka.EC2.Lens as Aws.Ec2.Lens
import qualified Amazonka.EC2.Types as Aws.Ec2.Types
import qualified Amazonka.EC2.Types.CpuOptions as Aws.Ec2.Types.CpuOptions
import qualified Amazonka.Lightsail as Aws.Lightsail
import qualified Amazonka.Lightsail.Lens as Aws.Lightsail.Lens
import qualified Amazonka.Lightsail.Types as Aws.Lightsail.Types
import qualified Amazonka.Lightsail.Types.Disk as Aws.Lightsail.Types.Disk
import qualified Autodocodec as ADC
import qualified Clompse.Types as Types
import Conduit ((.|))
import qualified Control.Lens as L
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Int (Int16, Int32)
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Float (double2Int)
import GHC.Generics (Generic)
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


awsEc2ListAllInstances
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(Aws.Region, Aws.Ec2.Instance)]
awsEc2ListAllInstances cfg = do
  regions <- awsEc2ListAllRegions cfg
  concat <$> mapM (awsEc2ListAllInstancesForRegion cfg) regions


awsEc2ListAllInstancesForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m [(Aws.Region, Aws.Ec2.Instance)]
awsEc2ListAllInstancesForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env Aws.Ec2.newDescribeInstances
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.describeInstancesResponse_reservations . L._Just)
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.reservation_instances . L._Just)
          .| CL.consume
  fmap (fmap (reg,)) . liftIO . Aws.runResourceT . C.runConduit $ prog


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


-- *** Instances with Security Groups


awsEc2ListAllInstancesWithSecurityGroups
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> m [(Aws.Region, Aws.Ec2.Instance, [Aws.Ec2.SecurityGroup])]
awsEc2ListAllInstancesWithSecurityGroups cfg = do
  instancesWithRegions <- awsEc2ListAllInstances cfg
  securityGroups <- awsEc2ListAllSecurityGroups cfg
  pure (fmap (\(r, i) -> (r, i, findSecurityGroups securityGroups i)) instancesWithRegions)
  where
    findSecurityGroups sgs i =
      let sids = catMaybes $ foldMap (fmap (L.^. Aws.Ec2.Lens.groupIdentifier_groupId)) (i L.^. Aws.Ec2.Lens.instance_securityGroups)
       in concatMap (\gi -> filter (\sg -> sg L.^. Aws.Ec2.Lens.securityGroup_groupId == gi) sgs) sids


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
  concat <$> mapM (awsLightsailListAllInstancesForRegion cfg) regions


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


ec2InstanceToServer :: Aws.Region -> Aws.Ec2.Instance -> Types.Server
ec2InstanceToServer region i@Aws.Ec2.Instance' {..} =
  Types.Server
    { Types._serverId = instanceId
    , Types._serverName = awsEc2InstanceName i
    , Types._serverCpu = fromIntegral <$> (Aws.Ec2.Types.CpuOptions.coreCount =<< cpuOptions)
    , Types._serverRam = Nothing
    , Types._serverDisk = Nothing
    , Types._serverState = ec2InstanceToServerState state
    , Types._serverCreatedAt = Just (Aws.Data.Time.fromTime launchTime)
    , Types._serverProvider = Types.ProviderAws
    , Types._serverRegion = Aws.fromRegion region
    , Types._serverType = Just (Aws.Ec2.fromInstanceType instanceType)
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


awsEc2InstanceName
  :: Aws.Ec2.Instance
  -> Maybe T.Text
awsEc2InstanceName i =
  let mTag = L.find (\t -> T.toLower (t L.^. Aws.Ec2.Lens.tag_key) == "name") =<< (i L.^. Aws.Ec2.Lens.instance_tags)
   in (L.^. Aws.Ec2.Lens.tag_value) <$> mTag


-- ** Lightsail


lightsailInstanceToServer :: Aws.Region -> Aws.Lightsail.Instance -> Types.Server
lightsailInstanceToServer region Aws.Lightsail.Types.Instance' {..} =
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
