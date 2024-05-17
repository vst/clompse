-- | This module provides top-level definitions to query remote AWS
-- API and provide responses as Clompse types.
module Clompse.Providers.Aws (
  AwsError (..),
  AwsConnection (..),
  listBucketsLightsail,
  listBucketsS3,
  listDnsRecordsLightsail,
  listDnsRecordsRoute53,
  listDomainsLightsail,
  listDomainsRoute53,
  listServersEc2,
  listServersLightsail,
) where

import Clompse.Providers.Aws.ApiAws (listBucketsS3, listDnsRecordsRoute53, listDomainsRoute53, listServersEc2)
import Clompse.Providers.Aws.ApiLightsail (listBucketsLightsail, listDnsRecordsLightsail, listDomainsLightsail, listServersLightsail)
import Clompse.Providers.Aws.Connection (AwsConnection (..))
import Clompse.Providers.Aws.Error (AwsError (..))

