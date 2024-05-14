{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Programs.ListDomains where

import qualified Autodocodec as ADC
import Clompse.Config (CloudConnection (..), CloudProfile (..), Config (..))
import qualified Clompse.Providers.Aws as Providers.Aws
import qualified Clompse.Providers.Do as Providers.Do
import qualified Clompse.Types as Types
import qualified Control.Concurrent.Async.Pool as Async
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.Csv as Cassava
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified System.IO
import qualified Zamazingo.Text as Z.Text


data ListDomainsResult = ListDomainsResult
  { _listDomainsResultProfile :: !T.Text
  , _listDomainsResultDomains :: ![Types.Domain]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ListDomainsResult)


instance ADC.HasCodec ListDomainsResult where
  codec =
    _codec ADC.<?> "List Domains Result Result"
    where
      _codec =
        ADC.object "ListDomainsResult" $
          ListDomainsResult
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _listDomainsResultProfile
            <*> ADC.requiredField "domains" "List of domains." ADC..= _listDomainsResultDomains


listDomains
  :: MonadIO m
  => Int
  -> Config
  -> m [ListDomainsResult]
listDomains ts Config {..} =
  liftIO . Async.withTaskGroup ts $ \tg -> Async.mapTasks tg (fmap listDomainsForCloudProfile _configCloudProfiles)


listDomainsForCloudProfile
  :: MonadIO m
  => CloudProfile
  -> m ListDomainsResult
listDomainsForCloudProfile CloudProfile {..} =
  ListDomainsResult _cloudProfileName . concat <$> mapM listDomainsForCloudConnection _cloudProfileConnections


listDomainsForCloudConnection
  :: MonadIO m
  => CloudConnection
  -> m [Types.Domain]
listDomainsForCloudConnection (CloudConnectionAws conn) = do
  eRecordsRoute53 <- runExceptT (Providers.Aws.listDomainsRoute53 conn)
  recordsRoute53 <- case eRecordsRoute53 of
    Left e -> _log ("    ERROR (AWS Route53 Domains): " <> Z.Text.tshow e) >> pure []
    Right records -> pure records
  eRecordsRouteLightsail <- runExceptT (Providers.Aws.listDomainsLightsail conn)
  recordsLightsail <- case eRecordsRouteLightsail of
    Left e -> _log ("    ERROR (AWS Lightsail Domains): " <> Z.Text.tshow e) >> pure []
    Right records -> pure records
  pure (recordsRoute53 <> recordsLightsail)
listDomainsForCloudConnection (CloudConnectionDo conn) = do
  eRecords <- runExceptT (Providers.Do.listDomains conn)
  case eRecords of
    Left e -> _log ("    ERROR (DO Domains): " <> Z.Text.tshow e) >> pure []
    Right records -> pure records
listDomainsForCloudConnection (CloudConnectionHetzner _conn) = do
  pure []


_log :: MonadIO m => T.Text -> m ()
_log =
  liftIO . TIO.hPutStrLn System.IO.stderr


type DomainsList = [DomainsListItem]


data DomainsListItem = DomainsListItem
  { _domainsListItemProfile :: !T.Text
  , _domainsListItemProvider :: !Types.Provider
  , _domainsListItemDomain :: !T.Text
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DomainsListItem)


instance ADC.HasCodec DomainsListItem where
  codec =
    _codec ADC.<?> "Domains List Item"
    where
      _codec =
        ADC.object "DomainsListItem" $
          DomainsListItem
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _domainsListItemProfile
            <*> ADC.requiredField "provider" "Provider of the object bucket." ADC..= _domainsListItemProvider
            <*> ADC.requiredField "domain" "Name of the object bucket." ADC..= _domainsListItemDomain


instance Cassava.ToNamedRecord DomainsListItem where
  toNamedRecord DomainsListItem {..} =
    Cassava.namedRecord
      [ "profile" Cassava..= _domainsListItemProfile
      , "provider" Cassava..= Types.providerCode _domainsListItemProvider
      , "domain" Cassava..= _domainsListItemDomain
      ]


instance Cassava.DefaultOrdered DomainsListItem where
  headerOrder _ =
    V.fromList
      [ "profile"
      , "provider"
      , "domain"
      ]


toDomainsList :: ListDomainsResult -> DomainsList
toDomainsList ListDomainsResult {..} =
  fmap (go _listDomainsResultProfile) _listDomainsResultDomains
  where
    go p Types.Domain {..} =
      DomainsListItem
        { _domainsListItemProfile = p
        , _domainsListItemProvider = _domainProvider
        , _domainsListItemDomain = _domainName
        }
