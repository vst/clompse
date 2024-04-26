{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Providers.Aws where

import qualified Amazonka as Aws
import qualified Amazonka.Auth as Aws.Auth
import qualified Amazonka.Data as Aws.Data
import qualified Amazonka.EC2 as Aws.Ec2
import qualified Amazonka.EC2.Lens as Aws.Ec2.Lens
import qualified Amazonka.EC2.Types as Aws.Ec2.Types
import qualified Amazonka.Lightsail as Aws.Lightsail
import qualified Amazonka.Lightsail.Lens as Aws.Lightsail.Lens
import qualified Autodocodec as ADC
import Conduit ((.|))
import Control.Applicative ((<|>))
import qualified Control.Lens as L
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
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
  -> m [Aws.Ec2.Instance]
awsEc2ListAllInstances cfg = do
  regions <- awsEc2ListAllRegions cfg
  concat <$> mapM (awsEc2ListAllInstancesForRegion cfg) regions


awsEc2ListAllInstancesForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m [Aws.Ec2.Instance]
awsEc2ListAllInstancesForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env Aws.Ec2.newDescribeInstances
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.describeInstancesResponse_reservations . L._Just)
          .| CL.concatMap (L.view $ Aws.Ec2.Lens.reservation_instances . L._Just)
          .| CL.consume
  liftIO . Aws.runResourceT . C.runConduit $ prog


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
  -> m [(Aws.Ec2.Instance, [Aws.Ec2.SecurityGroup])]
awsEc2ListAllInstancesWithSecurityGroups cfg = do
  instances <- awsEc2ListAllInstances cfg
  securityGroups <- awsEc2ListAllSecurityGroups cfg
  pure (fmap (\i -> (i, findSecurityGroups securityGroups i)) instances)
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
  -> m [Aws.Lightsail.Instance]
awsLightsailListAllInstances cfg = do
  regions <- awsLightsailListAllRegions cfg
  concat <$> mapM (awsLightsailListAllInstancesForRegion cfg) regions


awsLightsailListAllInstancesForRegion
  :: MonadIO m
  => MonadError AwsError m
  => AwsConnection
  -> Aws.Region
  -> m [Aws.Lightsail.Instance]
awsLightsailListAllInstancesForRegion cfg reg = do
  env <- (\x -> x {Aws.region = reg}) <$> _envFromConnection cfg
  let prog =
        Aws.paginate env Aws.Lightsail.newGetInstances
          .| CL.concatMap (L.view $ Aws.Lightsail.Lens.getInstancesResponse_instances . L._Just)
          .| CL.consume
  liftIO . Aws.runResourceT . C.runConduit $ prog


-- * Helpers


_envFromConnection
  :: MonadIO m
  => AwsConnection
  -> m Aws.Env
_envFromConnection AwsConnection {..} =
  Aws.newEnv (pure . Aws.Auth.fromKeys accessKeyId secretAccessKey)
  where
    accessKeyId = Aws.AccessKey (TE.encodeUtf8 _awsConnectionAccessKeyId)
    secretAccessKey = Aws.SecretKey (TE.encodeUtf8 _awsConnectionSecretAccessKey)


printAwsEc2InstanceWithSecurityGroup
  :: MonadIO m
  => (Aws.Ec2.Instance, [Aws.Ec2.SecurityGroup])
  -> m ()
printAwsEc2InstanceWithSecurityGroup (i, sgs) =
  let name = awsEc2InstanceName i
      secs = T.intercalate " " (fmap awsEc2SecurityToText sgs)
   in liftIO $ TIO.putStrLn (name <> ": " <> secs)


awsEc2InstanceName
  :: Aws.Ec2.Instance
  -> T.Text
awsEc2InstanceName i =
  let mTag = L.find (\t -> T.toLower (t L.^. Aws.Ec2.Lens.tag_key) == "name") =<< (i L.^. Aws.Ec2.Lens.instance_tags)
   in maybe (i L.^. Aws.Ec2.Lens.instance_instanceId) (L.^. Aws.Ec2.Lens.tag_value) mTag


awsEc2SecurityToText
  :: Aws.Ec2.SecurityGroup
  -> T.Text
awsEc2SecurityToText sg =
  let name = sg L.^. Aws.Ec2.Lens.securityGroup_groupName
      perms = fromMaybe [] $ sg L.^. Aws.Ec2.Lens.securityGroup_ipPermissions
   in name <> "=" <> T.intercalate "," (fmap awsEc2IpPermToText perms)


awsEc2IpPermToText
  :: Aws.Ec2.IpPermission
  -> T.Text
awsEc2IpPermToText x =
  let proto = x L.^. Aws.Ec2.Lens.ipPermission_ipProtocol
      portS = x L.^. Aws.Ec2.Lens.ipPermission_fromPort
      portE = x L.^. Aws.Ec2.Lens.ipPermission_toPort
   in proto <> "/" <> maybe "0" Z.Text.tshow portS <> "-" <> maybe "0" Z.Text.tshow portE


printAwsLightsailInstanceSecurity
  :: MonadIO m
  => Aws.Lightsail.Instance
  -> m ()
printAwsLightsailInstanceSecurity i =
  let name = fromMaybe (error "Missing instance name") (i L.^. Aws.Lightsail.Lens.instance_name <|> i L.^. Aws.Lightsail.Lens.instance_arn)
      ports = fromMaybe [] ((L.^. Aws.Lightsail.Lens.instanceNetworking_ports) =<< i L.^. Aws.Lightsail.Lens.instance_networking)
      portsText = T.intercalate " " (fmap portToText ports)
   in liftIO $ TIO.putStrLn (name <> ": " <> portsText)
  where
    portToText p =
      let pN = fromMaybe "<NA>" (p L.^. Aws.Lightsail.Lens.instancePortInfo_commonName)
          pP = maybe "<NA>" Aws.Lightsail.fromNetworkProtocol (p L.^. Aws.Lightsail.Lens.instancePortInfo_protocol)
          pS = p L.^. Aws.Lightsail.Lens.instancePortInfo_fromPort
          pE = p L.^. Aws.Lightsail.Lens.instancePortInfo_toPort
       in pN <> "=" <> pP <> "/" <> maybe "0" Z.Text.tshow pS <> "-" <> maybe "0" Z.Text.tshow pE
