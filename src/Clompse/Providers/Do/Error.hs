-- | This module provides DigitalOcean API error types and primitives.
module Clompse.Providers.Do.Error where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T


-- | This type represents errors that can occur when interacting with
-- or interpreting responses received from DigitalOcean API.
data DoError
  = -- | Error encountered during establishing connection to
    -- DigitalOcean API.
    DoErrorConnection !T.Text
  | -- | Error encountered during reading DigitalOcean API (command,
    -- arguments, error message).
    DoErrorCommand !FilePath ![T.Text] !T.Text
  | -- | Error encountered during parsing DigitalOcean API result
    -- (error message, payload being parsed).
    DoErrorParsing !T.Text !BL.ByteString
  | -- | Indicates an unknown/unexpected error.
    DoErrorUnknown !T.Text
  deriving (Eq, Show)
