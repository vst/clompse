{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Programs.ListServers where

import qualified Autodocodec as ADC
import Clompse.Config (CloudConnection (..), CloudProfile (..), Config (..))
import qualified Clompse.Providers.Aws as Providers
import qualified Clompse.Providers.Aws as Providers.Aws
import qualified Clompse.Providers.Do as Providers.Do
import qualified Clompse.Providers.Hetzner as Providers.Hetzner
import Clompse.Types (Server)
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import qualified System.IO
import qualified Zamazingo.Text as Z.Text


data ListServersResult = ListServersResult
  { _listServersResultProfile :: !T.Text
  , _listServersResultServers :: ![Server]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ListServersResult)


instance ADC.HasCodec ListServersResult where
  codec =
    _codec ADC.<?> "List Servers Result"
    where
      _codec =
        ADC.object "ListServersResult" $
          ListServersResult
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _listServersResultProfile
            <*> ADC.requiredField "servers" "List of servers." ADC..= _listServersResultServers


listServers
  :: MonadIO m
  => Config
  -> m [ListServersResult]
listServers Config {..} =
  mapM listServersForCloudProfile _configCloudProfiles


listServersForCloudProfile
  :: MonadIO m
  => CloudProfile
  -> m ListServersResult
listServersForCloudProfile CloudProfile {..} = do
  _log ("Running Profile: " <> _cloudProfileName)
  ListServersResult _cloudProfileName . concat <$> mapM listServersForCloudConnection _cloudProfileConnections


listServersForCloudConnection
  :: MonadIO m
  => CloudConnection
  -> m [Server]
listServersForCloudConnection (CloudConnectionAws conn) = do
  _log "  Running AWS EC2..."
  eServersEc2 <- runExceptT (Providers.Aws.awsEc2ListAllInstances conn)
  serversEc2 <- case eServersEc2 of
    Left e -> _log ("    ERROR: " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
  _log "  Running AWS Lightsail..."
  eServersLightsail <- runExceptT (Providers.Aws.awsLightsailListAllInstances conn)
  serversLightsail <- case eServersLightsail of
    Left e -> _log ("    ERROR: " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
  pure (fmap (uncurry Providers.ec2InstanceToServer) serversEc2 <> fmap (uncurry Providers.lightsailInstanceToServer) serversLightsail)
listServersForCloudConnection (CloudConnectionDo conn) = do
  _log "  Running DigitalOcean..."
  eServers <- runExceptT (Providers.Do.doListDroplets conn)
  case eServers of
    Left e -> _log ("    ERROR: " <> Z.Text.tshow e) >> pure []
    Right servers -> pure (fmap Providers.Do.toServer servers)
listServersForCloudConnection (CloudConnectionHetzner conn) = do
  _log "  Running Hetzner..."
  eServers <- runExceptT (Providers.Hetzner.hetznerListServers conn)
  case eServers of
    Left e -> _log ("    ERROR: " <> Z.Text.tshow e) >> pure []
    Right servers -> pure (fmap Providers.Hetzner.toServer servers)


_log :: MonadIO m => T.Text -> m ()
_log =
  liftIO . TIO.hPutStrLn System.IO.stderr
