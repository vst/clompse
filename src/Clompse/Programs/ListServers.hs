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
import qualified Clompse.Types as Types
import qualified Control.Concurrent.Async.Pool as Async
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.Csv as Cassava
import Data.Int (Int16, Int32)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Time as Time
import qualified Data.Vector as V
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
  => Int
  -> Config
  -> m [ListServersResult]
listServers ts Config {..} =
  liftIO . Async.withTaskGroup ts $ \tg -> Async.mapTasks tg (fmap listServersForCloudProfile _configCloudProfiles)


listServersForCloudProfile
  :: MonadIO m
  => CloudProfile
  -> m ListServersResult
listServersForCloudProfile CloudProfile {..} =
  ListServersResult _cloudProfileName . concat <$> mapM listServersForCloudConnection _cloudProfileConnections


listServersForCloudConnection
  :: MonadIO m
  => CloudConnection
  -> m [Server]
listServersForCloudConnection (CloudConnectionAws conn) = do
  eServersEc2 <- runExceptT (Providers.Aws.awsEc2ListAllInstances conn)
  serversEc2 <- case eServersEc2 of
    Left e -> _log ("    ERROR (AWS EC2): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
  eServersLightsail <- runExceptT (Providers.Aws.awsLightsailListAllInstances conn)
  serversLightsail <- case eServersLightsail of
    Left e -> _log ("    ERROR (AWS Lightsail): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
  pure (fmap (uncurry Providers.ec2InstanceToServer) serversEc2 <> fmap (uncurry Providers.lightsailInstanceToServer) serversLightsail)
listServersForCloudConnection (CloudConnectionDo conn) = do
  eServers <- runExceptT (Providers.Do.doListDroplets conn)
  case eServers of
    Left e -> _log ("    ERROR (DO): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure (fmap Providers.Do.toServer servers)
listServersForCloudConnection (CloudConnectionHetzner conn) = do
  eServers <- runExceptT (Providers.Hetzner.hetznerListServers conn)
  case eServers of
    Left e -> _log ("    ERROR (HETZNER): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure (fmap Providers.Hetzner.toServer servers)


_log :: MonadIO m => T.Text -> m ()
_log =
  liftIO . TIO.hPutStrLn System.IO.stderr


type ServerList = [ServerListItem]


data ServerListItem = ServerListItem
  { _serverListItemProfile :: !T.Text
  , _serverListItemProvider :: !Types.Provider
  , _serverListItemRegion :: !T.Text
  , _serverListItemId :: !T.Text
  , _serverListItemName :: !(Maybe T.Text)
  , _serverListItemState :: !Types.State
  , _serverListItemCpu :: !(Maybe Int16)
  , _serverListItemRam :: !(Maybe Int32)
  , _serverListItemDisk :: !(Maybe Int32)
  , _serverListItemType :: !(Maybe T.Text)
  , _serverListItemCreatedAt :: !(Maybe Time.UTCTime)
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ServerListItem)


instance ADC.HasCodec ServerListItem where
  codec =
    _codec ADC.<?> "Server List Item"
    where
      _codec =
        ADC.object "ServerListItem" $
          ServerListItem
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _serverListItemProfile
            <*> ADC.requiredField "provider" "Provider of the server." ADC..= _serverListItemProvider
            <*> ADC.requiredField "region" "Region of the server." ADC..= _serverListItemRegion
            <*> ADC.requiredField "id" "ID of the server." ADC..= _serverListItemId
            <*> ADC.optionalField "name" "Name of the server." ADC..= _serverListItemName
            <*> ADC.requiredField "state" "State of the server." ADC..= _serverListItemState
            <*> ADC.optionalField "cpu" "CPU of the server." ADC..= _serverListItemCpu
            <*> ADC.optionalField "ram" "RAM of the server." ADC..= _serverListItemRam
            <*> ADC.optionalField "disk" "Disk of the server." ADC..= _serverListItemDisk
            <*> ADC.optionalField "type" "Type of the server." ADC..= _serverListItemType
            <*> ADC.optionalField "created_at" "Creation time of the server." ADC..= _serverListItemCreatedAt


instance Cassava.ToNamedRecord ServerListItem where
  toNamedRecord ServerListItem {..} =
    Cassava.namedRecord
      [ "profile" Cassava..= _serverListItemProfile
      , "provider" Cassava..= Types.providerCode _serverListItemProvider
      , "region" Cassava..= _serverListItemRegion
      , "id" Cassava..= _serverListItemId
      , "name" Cassava..= _serverListItemName
      , "state" Cassava..= Types.stateCode _serverListItemState
      , "cpu" Cassava..= _serverListItemCpu
      , "ram" Cassava..= _serverListItemRam
      , "disk" Cassava..= _serverListItemDisk
      , "type" Cassava..= _serverListItemType
      , "created_at" Cassava..= fmap Z.Text.tshow _serverListItemCreatedAt
      ]


instance Cassava.DefaultOrdered ServerListItem where
  headerOrder _ =
    V.fromList
      [ "profile"
      , "provider"
      , "region"
      , "id"
      , "name"
      , "state"
      , "cpu"
      , "ram"
      , "disk"
      , "type"
      , "created_at"
      ]


toServerList :: ListServersResult -> ServerList
toServerList ListServersResult {..} =
  fmap (go _listServersResultProfile) _listServersResultServers
  where
    go p Types.Server {..} =
      ServerListItem
        { _serverListItemProfile = p
        , _serverListItemProvider = _serverProvider
        , _serverListItemRegion = _serverRegion
        , _serverListItemId = _serverId
        , _serverListItemName = _serverName
        , _serverListItemState = _serverState
        , _serverListItemCpu = _serverCpu
        , _serverListItemRam = _serverRam
        , _serverListItemDisk = _serverDisk
        , _serverListItemType = _serverType
        , _serverListItemCreatedAt = _serverCreatedAt
        }
