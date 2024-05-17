{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Programs.ListDomainRecords where

import qualified Autodocodec as ADC
import Clompse.Config (CloudConnection (..), CloudProfile (..), Config (..))
import qualified Clompse.Providers.Aws.ApiAws as Providers.Aws
import qualified Clompse.Providers.Aws.ApiLightsail as Providers.Aws
import qualified Clompse.Providers.Do as Providers.Do
import Clompse.Types (DnsRecord (_dnsRecordProvider))
import qualified Clompse.Types as Types
import qualified Control.Concurrent.Async.Pool as Async
import Control.Monad.Except (runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import qualified Data.Csv as Cassava
import Data.Int (Int32)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified System.IO
import qualified Zamazingo.Text as Z.Text


data ListDomainRecordsResult = ListDomainRecordsResult
  { _listDomainRecordsResultProfile :: !T.Text
  , _listDomainRecordsResultRecords :: ![Types.DnsRecord]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ListDomainRecordsResult)


instance ADC.HasCodec ListDomainRecordsResult where
  codec =
    _codec ADC.<?> "List Domains Records Result"
    where
      _codec =
        ADC.object "ListDomainRecordsResult" $
          ListDomainRecordsResult
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _listDomainRecordsResultProfile
            <*> ADC.requiredField "records" "List of records." ADC..= _listDomainRecordsResultRecords


listDomainRecords
  :: MonadIO m
  => Int
  -> Config
  -> m [ListDomainRecordsResult]
listDomainRecords ts Config {..} =
  liftIO . Async.withTaskGroup ts $ \tg -> Async.mapTasks tg (fmap listDomainRecordsForCloudProfile _configCloudProfiles)


listDomainRecordsForCloudProfile
  :: MonadIO m
  => CloudProfile
  -> m ListDomainRecordsResult
listDomainRecordsForCloudProfile CloudProfile {..} =
  ListDomainRecordsResult _cloudProfileName . concat <$> mapM listDomainRecordsForCloudConnection _cloudProfileConnections


listDomainRecordsForCloudConnection
  :: MonadIO m
  => CloudConnection
  -> m [Types.DnsRecord]
listDomainRecordsForCloudConnection (CloudConnectionAws conn) = do
  eRecordsRoute53 <- runExceptT (Providers.Aws.listDnsRecordsRoute53 conn)
  recordsRoute53 <- case eRecordsRoute53 of
    Left e -> _log ("    ERROR (Route53 Domain Records): " <> Z.Text.tshow e) >> pure []
    Right records -> pure records
  eRecordsLightsail <- runExceptT (Providers.Aws.listDnsRecordsLightsail conn)
  recordsLightsail <- case eRecordsLightsail of
    Left e -> _log ("    ERROR (Lightsail Domain Records): " <> Z.Text.tshow e) >> pure []
    Right records -> pure records
  pure (recordsRoute53 <> recordsLightsail)
listDomainRecordsForCloudConnection (CloudConnectionDo conn) = do
  eRecords <- runExceptT (Providers.Do.listDomainRecords conn)
  case eRecords of
    Left e -> _log ("    ERROR (DO Domain Records): " <> Z.Text.tshow e) >> pure []
    Right records -> pure records
listDomainRecordsForCloudConnection (CloudConnectionHetzner _conn) = do
  pure []


_log :: MonadIO m => T.Text -> m ()
_log =
  liftIO . TIO.hPutStrLn System.IO.stderr


type DomainRecordsList = [DomainRecordsListItem]


data DomainRecordsListItem = DomainRecordsListItem
  { _domainRecordsListItemProfile :: !T.Text
  , _domainRecordsListItemProvider :: !Types.Provider
  , _domainRecordsListItemDomain :: !T.Text
  , _domainRecordsListItemId :: !(Maybe T.Text)
  , _domainRecordsListItemType :: !T.Text
  , _domainRecordsListItemName :: !T.Text
  , _domainRecordsListItemValue :: !T.Text
  , _domainRecordsListItemPriority :: !(Maybe Int32)
  , _domainRecordsListItemPort :: !(Maybe Int32)
  , _domainRecordsListItemWeight :: !(Maybe Int32)
  , _domainRecordsListItemFlags :: !(Maybe Int32)
  , _domainRecordsListItemTtl :: !Int32
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec DomainRecordsListItem)


instance ADC.HasCodec DomainRecordsListItem where
  codec =
    _codec ADC.<?> "Domains Records List Item"
    where
      _codec =
        ADC.object "DomainRecordsListItem" $
          DomainRecordsListItem
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _domainRecordsListItemProfile
            <*> ADC.requiredField "provider" "Provider of the DNS service." ADC..= _domainRecordsListItemProvider
            <*> ADC.requiredField "domain" "Domain of the record." ADC..= _domainRecordsListItemDomain
            <*> ADC.optionalField "id" "ID of the record." ADC..= _domainRecordsListItemId
            <*> ADC.requiredField "type" "Type of the record." ADC..= _domainRecordsListItemType
            <*> ADC.requiredField "name" "Name of the record." ADC..= _domainRecordsListItemName
            <*> ADC.requiredField "value" "Value of the record." ADC..= _domainRecordsListItemValue
            <*> ADC.optionalField "priority" "Priority of the record." ADC..= _domainRecordsListItemPriority
            <*> ADC.optionalField "port" "Port of the record." ADC..= _domainRecordsListItemPort
            <*> ADC.optionalField "weight" "Weight of the record." ADC..= _domainRecordsListItemWeight
            <*> ADC.optionalField "flags" "Flags of the record." ADC..= _domainRecordsListItemFlags
            <*> ADC.requiredField "ttl" "TTL of the record." ADC..= _domainRecordsListItemTtl


instance Cassava.ToNamedRecord DomainRecordsListItem where
  toNamedRecord DomainRecordsListItem {..} =
    Cassava.namedRecord
      [ "profile" Cassava..= _domainRecordsListItemProfile
      , "provider" Cassava..= Types.providerCode _domainRecordsListItemProvider
      , "domain" Cassava..= _domainRecordsListItemDomain
      , "id" Cassava..= _domainRecordsListItemId
      , "type" Cassava..= _domainRecordsListItemType
      , "name" Cassava..= _domainRecordsListItemName
      , "value" Cassava..= _domainRecordsListItemValue
      , "priority" Cassava..= _domainRecordsListItemPriority
      , "port" Cassava..= _domainRecordsListItemPort
      , "weight" Cassava..= _domainRecordsListItemWeight
      , "flags" Cassava..= _domainRecordsListItemFlags
      , "ttl" Cassava..= _domainRecordsListItemTtl
      ]


instance Cassava.DefaultOrdered DomainRecordsListItem where
  headerOrder _ =
    V.fromList
      [ "profile"
      , "provider"
      , "domain"
      , "id"
      , "type"
      , "name"
      , "value"
      , "priority"
      , "port"
      , "weight"
      , "flags"
      , "ttl"
      ]


toDomainRecordsList :: ListDomainRecordsResult -> DomainRecordsList
toDomainRecordsList ListDomainRecordsResult {..} =
  fmap (go _listDomainRecordsResultProfile) _listDomainRecordsResultRecords
  where
    go p Types.DnsRecord {..} =
      DomainRecordsListItem
        { _domainRecordsListItemProfile = p
        , _domainRecordsListItemProvider = _dnsRecordProvider
        , _domainRecordsListItemDomain = _dnsRecordDomain
        , _domainRecordsListItemId = _dnsRecordId
        , _domainRecordsListItemType = _dnsRecordType
        , _domainRecordsListItemName = _dnsRecordName
        , _domainRecordsListItemValue = _dnsRecordValue
        , _domainRecordsListItemPriority = _dnsRecordPriority
        , _domainRecordsListItemPort = _dnsRecordPort
        , _domainRecordsListItemWeight = _dnsRecordWeight
        , _domainRecordsListItemFlags = _dnsRecordFlags
        , _domainRecordsListItemTtl = _dnsRecordTtl
        }
