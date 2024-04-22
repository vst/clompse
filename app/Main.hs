module Main where

import qualified Clompse.Cli as Cli
import System.Exit (exitWith)


main :: IO ()
main = Cli.cli >>= exitWith
