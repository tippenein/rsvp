{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Serve the API as an HTTP server.
module Rsvp.Server
  ( server
  , startApp
  ) where

import           System.Environment (lookupEnv)
import           Control.Monad.Log (Severity(..))
import qualified Data.List as List
import           GHC.Stats (getGCStatsEnabled)
import           Network.Wai.Handler.Warp
        (Port, Settings, defaultSettings, runSettings, setBeforeMainLoop,
        setPort)
import qualified Network.Wai.Middleware.RequestLogger as RL
import           Options.Applicative
       (ParserInfo, auto, eitherReader, execParser, fullDesc, header,
        help, helper, info, long, metavar, option, progDesc, switch, value)
import qualified Prometheus as Prom
import qualified Prometheus.Metric.GHC as Prom
import           Servant (serve)
import           Text.PrettyPrint.Leijen.Text (int, text)
import           Database.Persist.Sql (runSqlPool)

import           Rsvp.API (api)
import           Rsvp.Server.Config (makePool, Environment(..), Config(..), mkAuthConfig)
import           Rsvp.Server.Handlers (server)
import           Rsvp.Server.Instrument
       (defaultPrometheusSettings, prometheus, requestDuration)
import qualified Rsvp.Server.Logging as Log
import           Rsvp.Server.Models (doMigrations)

import           Protolude
import qualified Prelude


-- | Configuration for the application.
data CliConfig = CliConfig
  { port :: Port
  , accessLogs :: AccessLogs
  , logLevel :: Severity
  , enableGhcMetrics :: Bool
  } deriving (Show)

-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Run the service.
startApp :: IO ()
startApp = runApp =<< execParser options

options :: ParserInfo CliConfig
options = info (helper <*> parser) description
  where
    parser =
      CliConfig <$>
      option auto (fold [long "port", metavar "PORT", help "Port to listen on"]) <*>
      option
        (eitherReader parseAccessLogs)
        (fold
           [long "access-logs", help "How to log HTTP access", value Disabled]) <*>
      option
        (eitherReader
           (maybe (throwError (toS invalidLogLevel)) pure . Log.fromKeyword . toS))
        (fold
           [ long "log-level"
           , help "Minimum severity for log messages"
           , value Informational
           ]) <*>
      switch
        (fold
           [ long "ghc-metrics"
           , help "Export GHC metrics. Requires running with +RTS."
           ])
    invalidLogLevel = "Log level must be one of: " <> allLogLevels
    allLogLevels = fold . List.intersperse "," . map Log.toKeyword $ enumFrom minBound
    parseAccessLogs "none" = pure Disabled
    parseAccessLogs "basic" = pure Enabled
    parseAccessLogs "dev" = pure DevMode
    parseAccessLogs _ = throwError "One of 'none', 'basic', or 'dev'"
    description =
      fold
        [ fullDesc
        , progDesc "rsvp event system"
        , header "rsvp - yup"
        ]

runApp :: CliConfig -> IO ()
runApp config@CliConfig {..} = do
  rec
      let settings = warpSettings config
          middleware r =
            logging . prometheus defaultPrometheusSettings r "rsvp" $ app
          logging =
            case accessLogs of
              Disabled -> identity
              Enabled -> RL.logStdout
              DevMode -> RL.logStdoutDev
          app = serve api (server appConfig)
      requests <- Prom.registerIO requestDuration
      env <- lookupSetting "ENV" Development
      pool <- makePool env
      let appConfig = Config pool logLevel (mkAuthConfig pool) env
  runSqlPool doMigrations pool

  when enableGhcMetrics $ do
      statsEnabled <- getGCStatsEnabled
      unless statsEnabled $
        Log.withLogging logLevel $
        Log.log
          Warning
          (text
              "Exporting GHC metrics but GC stats not enabled. Re-run with +RTS -T.")
      void $ Prom.register Prom.ghcMetrics
  runSettings settings (middleware requests)

-- | Generate warp settings from config
--
-- Serve from a port and print out where we're serving from.
warpSettings :: CliConfig -> Settings
warpSettings CliConfig {..} =
  setBeforeMainLoop
    (Log.withLogging logLevel printPort)
    (setPort port defaultSettings)
  where
    printPort = Log.log Informational (text "Listening on :" `mappend` int port)

lookupSetting :: Read b => Prelude.String -> b -> IO b
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing ->
            return def
        Just str ->
            maybe (panic "failed to read from Env") return (readMaybe str)
