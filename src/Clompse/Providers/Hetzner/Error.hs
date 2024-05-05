-- | This module provides Hetzner error types and primitives.
module Clompse.Providers.Hetzner.Error where

import qualified Data.Text as T


-- | This type represents errors that can occur when interacting with
-- or interpreting responses received from Hetzner API.
newtype HetznerError
  = -- | Indicates an unknown/unexpected
    -- error.
    HetznerErrorUnknown T.Text
  deriving (Eq, Show)
