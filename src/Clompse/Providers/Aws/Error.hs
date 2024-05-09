-- | This module provides AWS API error types and primitives.
module Clompse.Providers.Aws.Error where

import qualified Data.Text as T


-- | This type represents errors that can occur when interacting with
-- or interpreting responses received from AWS API.
data AwsError
  = -- | Error encountered during parsing DigitalOcean API result
    -- (error message, payload being parsed).
    AwsErrorParsing !T.Text !T.Text
  | -- | Logical error while processing AWS API response.
    AwsErrorLogical !T.Text
  | -- | Indicates an unknown/unexpected error.
    AwsErrorUnknown !T.Text
  deriving (Eq, Show)
