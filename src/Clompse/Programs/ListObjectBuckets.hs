{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Clompse.Programs.ListObjectBuckets where

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
import qualified Data.Time as Time
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified System.IO
import qualified Zamazingo.Text as Z.Text


data ListObjectBucketsResult = ListObjectBucketsResult
  { _listObjectBucketsResultProfile :: !T.Text
  , _listObjectBucketsResultBuckets :: ![Types.ObjectBucket]
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ListObjectBucketsResult)


instance ADC.HasCodec ListObjectBucketsResult where
  codec =
    _codec ADC.<?> "List Object Buckets Result"
    where
      _codec =
        ADC.object "ListObjectBucketsResult" $
          ListObjectBucketsResult
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _listObjectBucketsResultProfile
            <*> ADC.requiredField "buckets" "List of object buckets." ADC..= _listObjectBucketsResultBuckets


listObjectBuckets
  :: MonadIO m
  => Int
  -> Config
  -> m [ListObjectBucketsResult]
listObjectBuckets ts Config {..} =
  liftIO . Async.withTaskGroup ts $ \tg -> Async.mapTasks tg (fmap listObjectBucketsForCloudProfile _configCloudProfiles)


listObjectBucketsForCloudProfile
  :: MonadIO m
  => CloudProfile
  -> m ListObjectBucketsResult
listObjectBucketsForCloudProfile CloudProfile {..} =
  ListObjectBucketsResult _cloudProfileName . concat <$> mapM listObjectBucketsForCloudConnection _cloudProfileConnections


listObjectBucketsForCloudConnection
  :: MonadIO m
  => CloudConnection
  -> m [Types.ObjectBucket]
listObjectBucketsForCloudConnection (CloudConnectionAws conn) = do
  eBucketsS3 <- runExceptT (Providers.Aws.awsListAllS3Buckets conn)
  bucketsS3 <- case eBucketsS3 of
    Left e -> _log ("    ERROR (AWS S3): " <> Z.Text.tshow e) >> pure []
    Right buckets -> pure (fmap (\(n, c) -> Types.ObjectBucket n Types.ProviderAws "S3" (Just c)) buckets)
  eBucketsLightsail <- runExceptT (Providers.Aws.awsListAllLightsailBuckets conn)
  bucketsLightsail <- case eBucketsLightsail of
    Left e -> _log ("    ERROR (AWS Lightsail): " <> Z.Text.tshow e) >> pure []
    Right buckets -> pure (fmap (\(n, c) -> Types.ObjectBucket n Types.ProviderAws "Lightsail" (Just c)) buckets)
  pure $ bucketsS3 <> bucketsLightsail
listObjectBucketsForCloudConnection (CloudConnectionDo _conn) = do
  eBucketSpaces <- runExceptT (Providers.Do.doListSpacesBuckets _conn)
  case eBucketSpaces of
    Left e -> _log ("    ERROR (DO Spaces): " <> Z.Text.tshow e) >> pure []
    Right buckets -> pure (fmap (\(n, c) -> Types.ObjectBucket n Types.ProviderDo "Spaces" (Just c)) buckets)
listObjectBucketsForCloudConnection (CloudConnectionHetzner _conn) = do
  pure []


_log :: MonadIO m => T.Text -> m ()
_log =
  liftIO . TIO.hPutStrLn System.IO.stderr


type ObjectBucketList = [ObjectBucketListItem]


data ObjectBucketListItem = ObjectBucketListItem
  { _objectBucketListItemProfile :: !T.Text
  , _objectBucketListItemProvider :: !Types.Provider
  , _objectBucketListItemProduct :: !T.Text
  , _objectBucketListItemName :: !T.Text
  , _objectBucketListItemCreatedAt :: !(Maybe Time.UTCTime)
  }
  deriving (Eq, Generic, Show)
  deriving (Aeson.FromJSON, Aeson.ToJSON) via (ADC.Autodocodec ObjectBucketListItem)


instance ADC.HasCodec ObjectBucketListItem where
  codec =
    _codec ADC.<?> "Object Bucket List Item"
    where
      _codec =
        ADC.object "ObjectBucketListItem" $
          ObjectBucketListItem
            <$> ADC.requiredField "profile" "Name of the cloud profile." ADC..= _objectBucketListItemProfile
            <*> ADC.requiredField "provider" "Provider of the object bucket." ADC..= _objectBucketListItemProvider
            <*> ADC.requiredField "product" "Product name." ADC..= _objectBucketListItemProduct
            <*> ADC.requiredField "name" "Name of the object bucket." ADC..= _objectBucketListItemName
            <*> ADC.optionalField "created_at" "Creation time of the object bucket." ADC..= _objectBucketListItemCreatedAt


instance Cassava.ToNamedRecord ObjectBucketListItem where
  toNamedRecord ObjectBucketListItem {..} =
    Cassava.namedRecord
      [ "profile" Cassava..= _objectBucketListItemProfile
      , "provider" Cassava..= Types.providerCode _objectBucketListItemProvider
      , "product" Cassava..= _objectBucketListItemProduct
      , "name" Cassava..= _objectBucketListItemName
      , "created_at" Cassava..= fmap Z.Text.tshow _objectBucketListItemCreatedAt
      ]


instance Cassava.DefaultOrdered ObjectBucketListItem where
  headerOrder _ =
    V.fromList
      [ "profile"
      , "provider"
      , "product"
      , "name"
      , "created_at"
      ]


toObjectBucketList :: ListObjectBucketsResult -> ObjectBucketList
toObjectBucketList ListObjectBucketsResult {..} =
  fmap (go _listObjectBucketsResultProfile) _listObjectBucketsResultBuckets
  where
    go p Types.ObjectBucket {..} =
      ObjectBucketListItem
        { _objectBucketListItemProfile = p
        , _objectBucketListItemProvider = _objectBucketProvider
        , _objectBucketListItemProduct = _objectBucketProduct
        , _objectBucketListItemName = _objectBucketName
        , _objectBucketListItemCreatedAt = _objectBucketCreatedAt
        }
