{-# LANGUAGE DataKinds #-}

module Hydra where

import Cardano (shouldDownload)
import Propellor
import Propellor.Base (combineModes, doesFileExist, readProcessEnv, (<.>), (</>))
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import System.Posix (ownerExecuteMode, ownerReadMode, ownerWriteMode)
import Data.List (isInfixOf)

setup :: User -> Property (MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish])
setup user =
    propertyList "Hydra node" $
        props
            & check
                (shouldDownload sha256 archivePath)
                ( userScriptProperty
                    user
                    ["curl -o " <> archivePath <> " -L " <> hydraNodeArchiveUrl]
                    `changesFileContent` archivePath
                )
                `describe` ("Hydra node " <> hydraVersion <> " archive downloaded")
            & check
                shouldUnpack
                ( userScriptProperty
                    user
                    ["unzip -qo " <> archivePath]
                    `changesFileContent` "/home/curry/hydra-node"
                    `requires` Apt.installed ["unzip"]
                )
                `describe` ("Hydra node " <> hydraVersion <> " archive unpacked")
            & File.mode "/home/curry/hydra-node" (combineModes [ownerReadMode, ownerWriteMode, ownerExecuteMode])
            & File.hasContent "/home/curry/hydra-node.environment" envFile
            & File.hasContent "/etc/systemd/system/hydra-node.service" serviceFile
            & Systemd.enabled "hydra-node"
            & Systemd.started "hydra-node"
  where
    hydraNodeArchiveUrl =
        "https://github.com/input-output-hk/hydra/releases/download"
            </> hydraVersion
            </> "hydra-x86_64-linux-"
            <> hydraVersion <.> "zip"

    sha256 = "760dbc71dfb01501003f80d6fd768ba9734202c908a70d39cb01b99f5abe5dc7"

    hydraExe = "hydra-node"

    hydraVersion = "0.15.0"

    shouldUnpack = do
        dir <- User.homedir user
        hasFile <- doesFileExist (dir </> hydraExe)
        if hasFile
            then
                not
                    . (hydraVersion `isInfixOf`)
                    <$> readProcessEnv (dir </> hydraExe) ["--version"] (Just [("LD_LIBRARY_PATH", dir)])
            else pure True

    archivePath = "hydra-node-" <> hydraVersion <.> "zip"

    userGrp = Group "curry"

    envFile =
        [ "SOCKETPATH=/home/curry/node.socket"
        , "HYDRA_SCRIPTS_TX_ID=7d998b617526d827dd69a495f5d5dc2c5e293b86a62ad61cb2fb5f2503cd87f0"
        ]

    serviceFile =
        [ "[Unit]"
        , "Description=Hydra node"
        , "After=multi-user.target"
        , ""
        , "[Service]"
        , "Type=simple"
        , "EnvironmentFile=/home/curry/hydra-node.environment"
        , "ExecStart=/home/curry/run-hydra.sh"
        , "KillSignal = SIGINT"
        , "RestartKillSignal = SIGINT"
        , "StandardOutput=journal"
        , "StandardError=journal"
        , "SyslogIdentifier=hydra-node"
        , ""
        , "LimitNOFILE=32768"
        , ""
        , "Restart=on-failure"
        , "RestartSec=15s"
        , "StartLimitIntervalSec=0"
        , "WorkingDirectory=~"
        , "User=curry"
        , "Group=curry"
        , ""
        , "[Install]"
        , "WantedBy=multi-user.target"
        ]
