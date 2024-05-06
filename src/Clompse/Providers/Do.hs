-- | This module provides top-level functions to query remote
-- DigitalOcean API and provide responses as Clompse types.
module Clompse.Providers.Do (
  DoError (..),
  DoConnection (..),
  listBuckets,
  listServers,
) where

import Clompse.Providers.Do.Api (listBuckets, listServers)
import Clompse.Providers.Do.Connection (DoConnection (..))
import Clompse.Providers.Do.Error (DoError (..))

