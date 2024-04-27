{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Providers.Hetzner where

import qualified Autodocodec as ADC
import qualified Clompse.Types as Types
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import Data.Int (Int16, Int32)
import qualified Data.List as List
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import GHC.Generics (Generic)
import qualified Hetzner.Cloud as Hetzner
import qualified Zamazingo.Text as Z.Text


-- * Connection


newtype HetznerConnection = HetznerConnection
  { _hetznerConnectionToken :: T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec HetznerConnection)


instance ADC.HasCodec HetznerConnection where
  codec =
    _codec ADC.<?> "Hetzner Connection"
    where
      _codec =
        ADC.object "HetznerConnection" $
          HetznerConnection
            <$> ADC.requiredField "token" "Hetzner API token." ADC..= _hetznerConnectionToken


-- * Error


newtype HetznerError
  = HetznerErrorUnknown T.Text
  deriving (Eq, Show)


-- * Operations


-- ** List Servers


-- TODO: Capture errors and lift to @MonadError HetznerError m@.
hetznerListServers
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Hetzner.Server]
hetznerListServers conn =
  Hetzner.streamToList (Hetzner.streamPages (Hetzner.getServers (_tokenFromConnection conn)))


-- ** List Firewalls


-- TODO: Capture errors and lift to @MonadError HetznerError m@.
hetznerListFirewalls
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [Hetzner.Firewall]
hetznerListFirewalls conn =
  Hetzner.streamToList (Hetzner.streamPages (Hetzner.getFirewalls (_tokenFromConnection conn)))


-- ** List Servers with Firewalls


-- TODO: Capture errors and lift to @MonadError HetznerError m@.
hetznerListServersWithFirewalls
  :: MonadIO m
  => MonadError HetznerError m
  => HetznerConnection
  -> m [(Hetzner.Server, [Hetzner.Firewall])]
hetznerListServersWithFirewalls conn = do
  servers <- hetznerListServers conn
  firewalls <- hetznerListFirewalls conn
  pure (fmap (\i -> (i, findFirewalls firewalls i)) servers)
  where
    findFirewalls fws i =
      let pns = fmap Hetzner.firewallStatusID (filter Hetzner.firewallIsApplied (Hetzner.publicNetworkFirewalls (Hetzner.serverPublicNetwork i)))
       in mapMaybe (\x -> List.find (\f -> Hetzner.firewallID f == x) fws) pns


-- * Helpers


toServer :: Hetzner.Server -> Types.Server
toServer Hetzner.Server {..} =
  Types.Server
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
    }


toServerId :: Hetzner.ServerID -> T.Text
toServerId (Hetzner.ServerID x) =
  Z.Text.tshow x


toServerCpu :: Hetzner.ServerType -> Int16
toServerCpu Hetzner.ServerType {..} =
  fromIntegral serverCores


toServerRam :: Hetzner.ServerType -> Int32
toServerRam Hetzner.ServerType {..} =
  fromIntegral serverMemory * 1024


toServerDisk :: Hetzner.ServerType -> Int32
toServerDisk Hetzner.ServerType {..} =
  fromIntegral serverDisk


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


_tokenFromConnection :: HetznerConnection -> Hetzner.Token
_tokenFromConnection =
  Hetzner.Token . TE.encodeUtf8 . _hetznerConnectionToken
