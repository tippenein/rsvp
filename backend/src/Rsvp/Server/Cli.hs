module Rsvp.Server.Cli where


import           Control.Monad.Log (Severity(..))
import qualified Data.List as List
import           Network.Wai.Handler.Warp (Port)
import           Options.Applicative
       (ParserInfo, auto, eitherReader, fullDesc, header,
        help, helper, info, long, metavar, option, progDesc, switch, value)
------------------------------
import qualified Rsvp.Server.Logging as Log

import           Protolude

-- | What level of access logs to show.
data AccessLogs
  = Disabled -- ^ Don't show access logs.
  | Enabled -- ^ Show Apache-style access logs.
  | DevMode -- ^ Show detailed, colorful access logs. Not suitable in production.
  deriving (Eq, Show)

-- | Configuration for the application.
data Config = Config
  { port :: Port
  , accessLogs :: AccessLogs
  , logLevel :: Severity
  , enableGhcMetrics :: Bool
  } deriving (Show)

options :: ParserInfo Config
options = info (helper <*> parser) description
  where
    parser =
      Config <$>
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
