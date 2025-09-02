module Amaru where

import Base (OS, OSNoInfo)
import Cardano (CardanoNetwork, networkName)
import Data.Bifunctor (second)
import qualified Data.List as List
import Propellor.Base
  ( Property,
    User,
    check,
    describe,
    doesDirectoryExist,
    ensureProperty,
    liftIO,
    property',
    propertyList,
    props,
    tightenTargets,
    userScriptProperty,
    (&),
    (<.>),
    (</>),
  )
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd
import Propellor.Property.User (homedir)

amaruInstalled :: User -> CardanoNetwork -> Property OS
amaruInstalled user network =
  property' "sensei service running" $ \w -> do
    dir <- liftIO $ homedir user
    let envFile = dir </> ".amaru" <.> networkName network <.> "environment"
    ensureProperty
      w
      ( propertyList "amaru service configured" $
          props
            & File.hasContent envFile (amaruEnv dir network)
            & File.hasContent "/etc/systemd/system/amaru.service" (amaruService envFile dir)
            & bootstrapped envFile user network
            & Systemd.enabled "amaru"
            & Systemd.restarted "amaru"
      )

bootstrapped :: FilePath -> User -> CardanoNetwork -> Property OSNoInfo
bootstrapped envFile user network =
  tightenTargets $
    check
      doesNotHaveDBs
      ( userScriptProperty
          user
          [ ". " <> envFile,
            "${HOME}/.local/bin/amaru bootstrap " <> "--config-dir ${HOME}/amaru/data" -- FIXME: should not depend on source
          ]
      )
      `describe` ("Amaru bootstrapped for " <> networkName network)
  where
    doesNotHaveDBs :: IO Bool
    doesNotHaveDBs = do
      -- NOTE: assumes envFile is defined and contains all we need
      ledgerDirEnv <- lookup "AMARU_LEDGER_DIR" . fmap (second tail . List.span (/= '=')) . List.lines <$> readFile envFile
      case ledgerDirEnv of
        Just ledgerDir -> not <$> doesDirectoryExist ledgerDir
        Nothing -> pure True

amaruEnv :: FilePath -> CardanoNetwork -> [String]
amaruEnv dir network =
  [ "AMARU_PEER_ADDRESS=localhost:3001",
    "AMARU_NETWORK=preview",
    "AMARU_LISTEN_ADDRESS=0.0.0.0:3000",
    "AMARU_LEDGER_DIR=" <> dir </> "amaru" </> "ledger" <.> networkName network <.> "db",
    "AMARU_CHAIN_DIR=" <> dir </> "amaru" </> "chain" <.> networkName network <.> "db"
  ]

amaruService :: String -> FilePath -> [String]
amaruService envFile dir =
  [ "[Unit]",
    "Description=Amaru",
    "After=multi-user.target",
    "",
    "[Service]",
    "EnvironmentFile=" <> envFile,
    "Type=simple",
    "ExecStart=" <> dir </> ".local" </> "bin" </> "amaru",
    "KillSignal = SIGINT",
    "RestartKillSignal = SIGINT",
    "StandardOutput=journal",
    "StandardError=journal",
    "SyslogIdentifier=amaru",
    "",
    "LimitNOFILE=32768",
    "",
    "Restart=on-failure",
    "RestartSec=15s",
    "StartLimitIntervalSec=0",
    "WorkingDirectory=~",
    "User=curry",
    "Group=curry",
    "",
    "[Install]",
    "WantedBy=multi-user.target"
  ]
