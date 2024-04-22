{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | This module provides top-level definitions for the CLI program.
module Clompse.Cli where

import qualified Autodocodec.Schema as ADC.Schema
import Clompse.Config (Config)
import qualified Clompse.Meta as Meta
import Control.Applicative ((<**>), (<|>))
import Control.Monad (join)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Options.Applicative as OA
import System.Exit (ExitCode (..))


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
    <|> commandVersion


-- * Commands


-- ** config


-- | Definition for @config@ CLI command.
commandConfig :: OA.Parser (IO ExitCode)
commandConfig = OA.hsubparser (OA.command "config" (OA.info parser infomod) <> OA.metavar "config")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Configuration commands." <> OA.footer "This command provides various configuration sub-commands."
    parser = commandConfigSchema


-- *** config schema


-- | Definition for @config schema@ CLI command.
commandConfigSchema :: OA.Parser (IO ExitCode)
commandConfigSchema = OA.hsubparser (OA.command "schema" (OA.info parser infomod) <> OA.metavar "schema")
  where
    infomod = OA.fullDesc <> infoModHeader <> OA.progDesc "Show configuration schema." <> OA.footer "This command prints the configuration JSON schema."
    parser = pure (BLC.putStrLn (Aeson.encode (ADC.Schema.jsonSchemaViaCodec @Config)) >> pure ExitSuccess)


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
