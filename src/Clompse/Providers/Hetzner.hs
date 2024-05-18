-- | This module provides top-level functions to query remote Hetzner
-- API and provide responses as Clompse types.
module Clompse.Providers.Hetzner (
  HetznerError (..),
  HetznerConnection (..),
  listDnsRecords,
  listDomains,
  listServers,
) where

import Clompse.Providers.Hetzner.Api (listDnsRecords, listDomains, listServers)
import Clompse.Providers.Hetzner.Connection (HetznerConnection (..))
import Clompse.Providers.Hetzner.Error (HetznerError (..))

