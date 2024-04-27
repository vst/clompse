{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides top-level definitions for the CLI program.
module Clompse.Cli where

import qualified Autodocodec.Schema as ADC.Schema
import Clompse.Config (Config, readConfigFile)
import qualified Clompse.Meta as Meta
import qualified Clompse.Programs.ListServers as Programs
import qualified Clompse.Types as Types
import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Format.Numbers as Fmt.Number
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))
import qualified Text.Layout.Table as Tab
import qualified Zamazingo.Text as Z.Text


-- * Entrypoint


-- | CLI program entrypoint.
cli :: IO ExitCode
cli =
  join (OA.execParser (OA.info opts desc))
  where
    opts = optProgram <**> infoOptVersion <**> OA.helper
    desc =
      OA.fullDesc
        <> OA.progDesc "Top Level Commands"
        <> infoModHeader
        <> infoModFooter


-- * Program


-- | Option parser for top-level commands.
optProgram :: OA.Parser (IO ExitCode)
optProgram =
  commandConfig
    <|> commandServer
    <|> commandVersion


-- * Commands


-- ** config


-- | Definition for @config@ CLI command.
commandConfig :: OA.Parser (IO ExitCode)
commandConfig = OA.hsubparser (OA.command "config" (OA.info parser infomod) <> OA.metavar "config")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Configuration commands." <> OA.footer "This command provides various configuration sub-commands."
    parser =
      commandConfigSchema
        <|> commandConfigPrint


-- *** config schema


-- | Definition for @config schema@ CLI command.
commandConfigSchema :: OA.Parser (IO ExitCode)
commandConfigSchema = OA.hsubparser (OA.command "schema" (OA.info parser infomod) <> OA.metavar "schema")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Show configuration schema." <> OA.footer "This command prints the configuration JSON schema."
    parser = pure (BLC.putStrLn (Aeson.encode (ADC.Schema.jsonSchemaViaCodec @Config)) >> pure ExitSuccess)


-- *** config print


-- | Definition for @config print@ CLI command.
commandConfigPrint :: OA.Parser (IO ExitCode)
commandConfigPrint = OA.hsubparser (OA.command "print" (OA.info parser infomod) <> OA.metavar "print")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Read, validate and print configuration." <> OA.footer "This command reads, validates and prints the given configuration."
    parser =
      doConfigPrint
        <$> OA.strOption (OA.short 'f' <> OA.long "file" <> OA.metavar "FILE" <> OA.help "Configuration file to read.")


-- | @config print@ CLI command program.
doConfigPrint :: FilePath -> IO ExitCode
doConfigPrint fp = do
  eCfg <- readConfigFile fp
  case eCfg of
    Left err -> TIO.putStrLn ("Error reading configuration: " <> err) >> pure (ExitFailure 1)
    Right cfg -> BLC.putStrLn (Aeson.encode cfg) >> pure ExitSuccess


-- ** server


-- | Definition for @server@ CLI command.
commandServer :: OA.Parser (IO ExitCode)
commandServer = OA.hsubparser (OA.command "server" (OA.info parser infomod) <> OA.metavar "server")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Server commands." <> OA.footer "This command provides various server commands."
    parser =
      commandServerList


-- *** server list


-- | Definition for @server list@ CLI command.
commandServerList :: OA.Parser (IO ExitCode)
commandServerList = OA.hsubparser (OA.command "list" (OA.info parser infomod) <> OA.metavar "list")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "List servers." <> OA.footer "This command lists servers."
    parser =
      doServerList
        <$> OA.strOption (OA.short 'c' <> OA.long "config" <> OA.metavar "CONFIG" <> OA.help "Configuration file to use.")
        <*> OA.switch (OA.short 'j' <> OA.long "json" <> OA.help "Format output in JSON.")


-- | @server list@ CLI command program.
doServerList :: FilePath -> Bool -> IO ExitCode
doServerList fp json = do
  eCfg <- readConfigFile fp
  case eCfg of
    Left err -> TIO.putStrLn ("Error reading configuration: " <> err) >> pure (ExitFailure 1)
    Right cfg -> do
      servers <- Programs.listServers cfg
      (if json then doServerListPrintJson else doServerListTabulate) servers
      pure ExitSuccess


-- | Prints list server results in JSON format.
doServerListPrintJson :: [Programs.ListServersResult] -> IO ()
doServerListPrintJson = BLC.putStrLn . Aeson.encode


-- | Prints list server results in tabular format.
doServerListTabulate :: [Programs.ListServersResult] -> IO ()
doServerListTabulate rs =
  let cs =
        [ Tab.numCol
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.numCol
        , Tab.numCol
        , Tab.numCol
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        , Tab.column Tab.expand Tab.left Tab.noAlign Tab.noCutMark
        ]
      hs =
        Tab.titlesH
          [ "#"
          , "Profile"
          , "Provider"
          , "Region"
          , "ID"
          , "Name"
          , "State"
          , "CPU"
          , "Ram"
          , "Disk"
          , "Type"
          , "Created"
          ]
      mkRows i p Types.Server {..} =
        Tab.rowG . fmap T.unpack $
          [ formatIntegral i
          , p
          , Types.providerCode _serverProvider
          , _serverRegion
          , _serverId
          , fromMaybe "<unknown>" _serverName
          , Types.stateCode _serverState
          , maybe "<unknown>" formatIntegral _serverCpu
          , maybe "<unknown>" formatIntegral _serverRam
          , maybe "<unknown>" formatIntegral _serverDisk
          , fromMaybe "<unknown>" _serverType
          , maybe "<unknown>" Z.Text.tshow _serverCreatedAt
          ]
      rows =
        fmap (\(i, (p, s)) -> mkRows i p s) . zip [1 :: Int ..] $
          concatMap (\(Programs.ListServersResult p is) -> fmap (p,) is) rs
   in putStrLn $ Tab.tableString cs Tab.unicodeS hs rows


formatIntegral :: Integral a => a -> T.Text
formatIntegral =
  Fmt.Number.prettyI (Just ',') . fromIntegral


-- ** version


-- | Definition for @version@ CLI command.
commandVersion :: OA.Parser (IO ExitCode)
commandVersion = OA.hsubparser (OA.command "version" (OA.info parser infomod) <> OA.metavar "version")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Show version and build information." <> OA.footer "This command shows version and build information."
    parser =
      doVersion
        <$> OA.switch (OA.short 'j' <> OA.long "json" <> OA.help "Format output in JSON.")


-- | @version@ CLI command program.
doVersion :: Bool -> IO ExitCode
doVersion True = BLC.putStrLn (Aeson.encode Meta.buildInfo) >> pure ExitSuccess
doVersion False = TIO.putStrLn (Meta.prettyBuildInfo Meta.buildInfo) >> pure ExitSuccess


-- * Helpers


-- | Version option parser.
infoOptVersion :: OA.Parser (a -> a)
infoOptVersion =
  OA.infoOption Meta.versionString $
    OA.short 'v'
      <> OA.long "version"
      <> OA.help "Show application version and exit"


-- | Header 'OA.InfoMod'.
infoModHeader :: OA.InfoMod a
infoModHeader =
  OA.header (T.unpack (Meta.name <> " - " <> Meta.title <> " v" <> Meta.versionText))


-- | Footer 'OA.InfoMod'.
infoModFooter :: OA.InfoMod a
infoModFooter =
  OA.footer "See <https://github.com/vst/clompse> for help and feedback."


-- | Tests a parser with given arguments.
runParserTest :: OA.Parser a -> [String] -> OA.ParserResult a
runParserTest parser = OA.execParserPure (OA.prefs prefs) (OA.info (parser <**> OA.helper) infomod)
  where
    prefs = OA.showHelpOnError <> OA.helpLongEquals <> OA.helpShowGlobals
    infomod = OA.fullDesc <> OA.progDesc "Test Parser" <> OA.header "testparser - especially for doctests"


-- | Tests an IO parser with given arguments.
runParserTestIO :: OA.Parser (IO a) -> [String] -> IO (Either String ())
runParserTestIO p as = case runParserTest p as of
  OA.Success _ -> pure (Right ())
  OA.Failure f -> pure (Left (show f))
  OA.CompletionInvoked _ -> pure (Right ())
