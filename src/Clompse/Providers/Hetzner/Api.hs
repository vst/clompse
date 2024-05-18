{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | This module provides functions to query remote Hetzner API and
-- convert responses to Clompse types.
module Clompse.Providers.Hetzner.Api where

import Clompse.Providers.Hetzner.Connection (HetznerConnection (..), hetznerConnectionToken, hetznerConnectionTokenDns)
import Clompse.Providers.Hetzner.Error (HetznerError)
import qualified Clompse.Types as Types
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe, maybeToList)
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Hetzner.Cloud as Hetzner
import qualified Hetzner.DNS as Hetzner
import qualified Net.IPv4
import qualified Net.IPv6
import qualified Zamazingo.Net as Z.Net
import qualified Zamazingo.Text as Z.Text


-- * Operations


-- | Lists all servers available in the Hetzner account associated
-- with the given connection as Clompse servers.
listServers
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Types.Server]
listServers = do
  fmap (fmap toServer) . apiListServersFirewalls


-- | Lists all domains managed in the Hetzner account associated with
-- the given connection.
listDomains
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Types.Domain]
listDomains conn = do
  zones <- maybe (pure []) (Hetzner.streamToList . Hetzner.streamPages . Hetzner.getZones) (hetznerConnectionTokenDns conn)
  pure $ fmap toDomain zones
  where
    toDomain Hetzner.Zone {..} =
      Types.Domain
        { _domainName = zoneName
        , _domainProvider = Types.ProviderHetzner
        }


listDnsRecords
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Types.DnsRecord]
listDnsRecords conn = do
  zones <- maybe (pure []) (Hetzner.streamToList . Hetzner.streamPages . Hetzner.getZones) (hetznerConnectionTokenDns conn)
  records <- concat <$> traverse (\Hetzner.Zone {..} -> liftIO $ fmap (zoneName,) <$> maybe (pure []) (`Hetzner.getRecords` Just zoneID) (hetznerConnectionTokenDns conn)) zones
  pure $ fmap toDnsRecord records
  where
    toDnsRecord (zoneName, Hetzner.Record {..}) =
      let (Hetzner.RecordID _dnsRecordId) = recordID
          _dnsRecordType = Z.Text.tshow recordType
          _dnsRecordName = recordName
          _dnsRecordValue = recordValue
          _dnsRecordPriority = Nothing
          _dnsRecordPort = Nothing
          _dnsRecordWeight = Nothing
          _dnsRecordFlags = Nothing
          _dnsRecordTtl = fromIntegral recordTTL
       in Types.DnsRecord
            { _dnsRecordProvider = Types.ProviderHetzner
            , _dnsRecordDomain = zoneName
            , _dnsRecordId = Just _dnsRecordId
            , ..
            }


-- * Helpers


-- ** Hetzner API Helpers


-- | Lists all servers available in the Hetzner account associated
-- with the given connection.
--
-- __TODO:__ Capture errors and lift to @MonadError HetznerError m@.
apiListServers
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Hetzner.Server]
apiListServers =
  Hetzner.streamToList . Hetzner.streamPages . Hetzner.getServers . hetznerConnectionToken


-- | Lists all firewalls available in the Hetzner account associated
-- with the given connection.
--
-- __TODO:__ Capture errors and lift to @MonadError HetznerError m@.
apiListFirewalls
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Hetzner.Firewall]
apiListFirewalls =
  Hetzner.streamToList . Hetzner.streamPages . Hetzner.getFirewalls . hetznerConnectionToken


-- | Lists all servers available in the Hetzner account associated
-- with the given connection along with their firewalls information,
-- if any.
--
-- __TODO:__ Capture errors and lift to @MonadError HetznerError m@.
apiListServersFirewalls
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [(Hetzner.Server, [Hetzner.Firewall])]
apiListServersFirewalls conn = do
  servers <- apiListServers conn
  firewalls <- apiListFirewalls conn
  pure (fmap (\i -> (i, findFirewalls firewalls i)) servers)
  where
    findFirewalls fws i =
      let pns = fmap Hetzner.firewallStatusID (filter Hetzner.firewallIsApplied (Hetzner.publicNetworkFirewalls (Hetzner.serverPublicNetwork i)))
       in mapMaybe (\x -> List.find (\f -> Hetzner.firewallID f == x) fws) pns


-- ** Data Helpers


-- | Converts a given Hetzner server to a Clompse server.
toServer :: (Hetzner.Server, [Hetzner.Firewall]) -> Types.Server
toServer (srv@Hetzner.Server {..}, fws) =
  let
   in Types.Server
        { Types._serverId = toServerId serverID
        , Types._serverName = Just serverName
        , Types._serverCpu = Just (toServerCpu serverType)
        , Types._serverRam = Just (toServerRam serverType)
        , Types._serverDisk = Just (toServerDisk serverType)
        , Types._serverState = toServerState serverStatus
        , Types._serverCreatedAt = Just (Time.zonedTimeToUTC serverCreated)
        , Types._serverProvider = Types.ProviderHetzner
        , Types._serverRegion = Hetzner.locationName . Hetzner.datacenterLocation $ serverDatacenter
        , Types._serverType = Just (Hetzner.serverTypeDescription serverType)
        , Types._serverIpInfo = toServerIpInfo srv
        , Types._serverFirewalls = fmap toFirewall fws
        }


-- | Extracts the IP information from a given Hetzner server.
toServerIpInfo :: Hetzner.Server -> Types.ServerIpInfo
toServerIpInfo Hetzner.Server {..} =
  Types.ServerIpInfo
    { _serverIpInfoStaticIpv4 = [] -- TODO: hetzner library does not provide this information.
    , _serverIpInfoStaticIpv6 = [] -- TODO: hetzner library does not provide this information.
    , _serverIpInfoPrivateIpv4 = [] -- TODO: hetzner library does not provide this information.
    , _serverIpInfoPrivateIpv6 = [] -- TODO: hetzner library does not provide this information.
    , _serverIpInfoPublicIpv4 = maybeToList (Z.Net.MkIPv4 . Hetzner.publicIP <$> Hetzner.publicIPv4 serverPublicNetwork)
    , _serverIpInfoPublicIpv6 = foldMap (fmap (Z.Net.MkIPv6 . Hetzner.publicIP) . Hetzner.reverseDNS) (Hetzner.publicIPv6 serverPublicNetwork)
    }


-- | Extracts the server ID from a given Hetzner server ID as a
-- textual value.
toServerId :: Hetzner.ServerID -> T.Text
toServerId (Hetzner.ServerID x) =
  Z.Text.tshow x


-- | Extracts the CPU count from a given Hetzner server type.
toServerCpu :: Hetzner.ServerType -> Int16
toServerCpu Hetzner.ServerType {..} =
  fromIntegral serverCores


-- | Extracts the RAM size from a given Hetzner server type.
toServerRam :: Hetzner.ServerType -> Int32
toServerRam Hetzner.ServerType {..} =
  fromIntegral serverMemory * 1024


-- | Extracts the disk size from a given Hetzner server type.
toServerDisk :: Hetzner.ServerType -> Int32
toServerDisk Hetzner.ServerType {..} =
  fromIntegral serverDisk


-- | Converts a given Hetzner server status to a Clompse server state.
toServerState :: Hetzner.ServerStatus -> Types.State
toServerState Hetzner.Initializing = Types.StateCreating
toServerState Hetzner.Starting = Types.StateCreating
toServerState Hetzner.Running = Types.StateRunning
toServerState Hetzner.Stopping = Types.StateStopping
toServerState Hetzner.Off = Types.StateStopped
toServerState Hetzner.Deleting = Types.StateTerminating
toServerState Hetzner.Rebuilding = Types.StateRebuilding
toServerState Hetzner.Migrating = Types.StateMigrating
toServerState Hetzner.StatusUnknown = Types.StateUnknown


-- | Converts a given Hetzner firewall to a Clompse firewall.
toFirewall :: Hetzner.Firewall -> Types.Firewall
toFirewall Hetzner.Firewall {..} =
  Types.Firewall
    { _firewallId = Z.Text.tshow (fwId firewallID)
    , _firewallName = Just firewallName
    , _firewallRulesInbound = fmap toFirewallRule (filter ((==) Hetzner.TrafficIn . Hetzner.firewallRuleDirection) firewallRules)
    , _firewallRulesOutbound = fmap toFirewallRule (filter ((==) Hetzner.TrafficOut . Hetzner.firewallRuleDirection) firewallRules)
    , _firewallCreatedAt = Just (Time.zonedTimeToUTC firewallCreated)
    }
  where
    fwId (Hetzner.FirewallID x) = x


-- | Converts a given Hetzner firewall rule to a Clompse firewall
-- rule.
toFirewallRule :: Hetzner.FirewallRule -> Types.FirewallRule
toFirewallRule Hetzner.FirewallRule {..} =
  Types.FirewallRule
    { _firewallRuleProtocol = protocol
    , _firewallRulePorts = ports
    , _firewallRuleEntities = entities
    }
  where
    protocol = case firewallRuleProtocol of
      Hetzner.FirewallRuleTCP _ -> "tcp"
      Hetzner.FirewallRuleUDP _ -> "udp"
      Hetzner.FirewallRuleICMP -> "icmp"
      Hetzner.FirewallRuleESP -> "esp"
      Hetzner.FirewallRuleGRE -> "gre"
    entities = NE.toList $ fmap (either Net.IPv4.encodeRange Net.IPv6.encodeRange) firewallRuleIPs
    ports = case firewallRuleProtocol of
      Hetzner.FirewallRuleTCP (Hetzner.PortRange f t) ->
        [Types.FirewallRulePorts {_firewallRulePortsFrom = fromIntegral f, _firewallRulePortsTo = fromIntegral t}]
      Hetzner.FirewallRuleUDP (Hetzner.PortRange f t) ->
        [Types.FirewallRulePorts {_firewallRulePortsFrom = fromIntegral f, _firewallRulePortsTo = fromIntegral t}]
      _ -> []
