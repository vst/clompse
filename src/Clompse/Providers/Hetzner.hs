{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Clompse.Providers.Hetzner where

import qualified Autodocodec as ADC
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
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


printServerFirewall
  :: MonadIO m
  => (Hetzner.Server, [Hetzner.Firewall])
  -> m ()
printServerFirewall (i, sgs) =
  let name = Hetzner.serverName i
      secs = T.intercalate " " (fmap firewallToText sgs)
   in liftIO $ TIO.putStrLn (name <> ": " <> secs)


firewallToText
  :: Hetzner.Firewall
  -> T.Text
firewallToText fw =
  let name = Hetzner.firewallName fw
      rules = fmap ruleToText (Hetzner.firewallRules fw)
   in name <> "=" <> T.intercalate "," rules


ruleToText :: Hetzner.FirewallRule -> T.Text
ruleToText rule =
  let dir = Z.Text.tshow (Hetzner.firewallRuleDirection rule)
      proto = Z.Text.tshow (Hetzner.firewallRuleProtocol rule)
      ips = fmap (either Z.Text.tshow Z.Text.tshow) (NE.toList (Hetzner.firewallRuleIPs rule))
   in dir <> "+" <> proto <> "://" <> T.intercalate ";" ips


_tokenFromConnection :: HetznerConnection -> Hetzner.Token
_tokenFromConnection =
  Hetzner.Token . TE.encodeUtf8 . _hetznerConnectionToken
