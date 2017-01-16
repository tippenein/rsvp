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
import qualified Rsvp.Server.Cli as Cli

import           Protolude
import qualified Prelude


-- | Run the service.
startApp :: IO ()
startApp = runApp =<< execParser Cli.options


runApp :: Cli.Config -> IO ()
runApp config@Cli.Config {..} = do
  rec
      let settings = warpSettings config
          middleware r =
            logging . prometheus defaultPrometheusSettings r "rsvp" $ app
          logging =
            case accessLogs of
              Cli.Disabled -> identity
              Cli.Enabled -> RL.logStdout
              Cli.DevMode -> RL.logStdoutDev
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
warpSettings :: Cli.Config -> Settings
warpSettings Cli.Config {..} =
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
