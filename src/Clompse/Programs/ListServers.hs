{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Programs.ListServers where

import qualified Autodocodec as ADC
import Clompse.Config (CloudConnection (..), CloudProfile (..), Config (..))
import qualified Clompse.Providers.Aws as Providers.Aws
import qualified Clompse.Providers.Do as Providers.Do
import qualified Clompse.Providers.Hetzner as Providers.Hetzner
import Clompse.Types (Server, ServerIpInfo (..))
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
import qualified Zamazingo.Net as Z.Net
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
  eServersEc2 <- runExceptT (Providers.Aws.listServersEc2 conn)
  serversEc2 <- case eServersEc2 of
    Left e -> _log ("    ERROR (AWS EC2): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
  eServersLightsail <- runExceptT (Providers.Aws.listServersLightsail conn)
  serversLightsail <- case eServersLightsail of
    Left e -> _log ("    ERROR (AWS Lightsail): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
  pure (serversEc2 <> serversLightsail)
listServersForCloudConnection (CloudConnectionDo conn) = do
  eServers <- runExceptT (Providers.Do.listServers conn)
  case eServers of
    Left e -> _log ("    ERROR (DO): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers
listServersForCloudConnection (CloudConnectionHetzner conn) = do
  eServers <- runExceptT (Providers.Hetzner.listServers conn)
  case eServers of
    Left e -> _log ("    ERROR (HETZNER): " <> Z.Text.tshow e) >> pure []
    Right servers -> pure servers


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
  , _serverListItemIPv4Static :: ![Z.Net.IPv4]
  , _serverListItemIPv4Public :: ![Z.Net.IPv4]
  , _serverListItemIPv4Private :: ![Z.Net.IPv4]
  , _serverListItemIPv6Static :: ![Z.Net.IPv6]
  , _serverListItemIPv6Public :: ![Z.Net.IPv6]
  , _serverListItemIPv6Private :: ![Z.Net.IPv6]
  , _serverListItemFirewalls :: ![Types.Firewall]
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
            <*> ADC.requiredField "ipv4_static" "Static IPv4 addresses." ADC..= _serverListItemIPv4Static
            <*> ADC.requiredField "ipv4_public" "Public IPv4 addresses." ADC..= _serverListItemIPv4Public
            <*> ADC.requiredField "ipv4_private" "Private IPv4 addresses." ADC..= _serverListItemIPv4Private
            <*> ADC.requiredField "ipv6_static" "Ptatic IPv6 addresses." ADC..= _serverListItemIPv6Static
            <*> ADC.requiredField "ipv6_public" "Public IPv6 addresses." ADC..= _serverListItemIPv6Public
            <*> ADC.requiredField "ipv6_private" "Private IPv6 addresses." ADC..= _serverListItemIPv6Private
            <*> ADC.requiredField "firewalls" "Firewall configurations." ADC..= _serverListItemFirewalls


instance Cassava.ToNamedRecord ServerListItem where
  toNamedRecord ServerListItem {..} =
    let reportIp4s = filterMaybe (not . T.null) . T.intercalate "," . fmap Z.Net.ipv4ToText
        reportIp6s = filterMaybe (not . T.null) . T.intercalate "," . fmap Z.Net.ipv6ToText
     in Cassava.namedRecord
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
          , "ipv4_static" Cassava..= reportIp4s _serverListItemIPv4Static
          , "ipv4_public" Cassava..= reportIp4s _serverListItemIPv4Public
          , "ipv4_private" Cassava..= reportIp4s _serverListItemIPv4Private
          , "ipv6_static" Cassava..= reportIp6s _serverListItemIPv6Static
          , "ipv6_public" Cassava..= reportIp6s _serverListItemIPv6Public
          , "ipv6_private" Cassava..= reportIp6s _serverListItemIPv6Private
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
      , "ipv4_static"
      , "ipv4_public"
      , "ipv4_private"
      , "ipv6_static"
      , "ipv6_public"
      , "ipv6_private"
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
        , _serverListItemIPv4Static = _serverIpInfoStaticIpv4 _serverIpInfo
        , _serverListItemIPv4Public = _serverIpInfoPublicIpv4 _serverIpInfo
        , _serverListItemIPv4Private = _serverIpInfoPrivateIpv4 _serverIpInfo
        , _serverListItemIPv6Static = _serverIpInfoStaticIpv6 _serverIpInfo
        , _serverListItemIPv6Public = _serverIpInfoPublicIpv6 _serverIpInfo
        , _serverListItemIPv6Private = _serverIpInfoPrivateIpv6 _serverIpInfo
        , _serverListItemFirewalls = _serverFirewalls
        }


filterMaybe :: (a -> Bool) -> a -> Maybe a
filterMaybe p a
  | p a = Just a
  | otherwise = Nothing
