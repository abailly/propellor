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
                (shouldDownload sha256 ("/home/curry" </> archivePath))
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
            & File.hasContent "/home/curry/hydra-run.sh" hydraRunFile
              `requires` Systemd.stopped "hydra-node"
            & File.mode "/home/curry/hydra-run.sh" (combineModes [ownerReadMode, ownerWriteMode, ownerExecuteMode])
            & File.ownerGroup "/home/curry/hydra-run.sh" user userGrp
            & File.hasContent "/etc/systemd/system/hydra-node.service" serviceFile
            & Systemd.enabled "hydra-node"
            & Systemd.restarted "hydra-node"
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
        , "HYDRA_SCRIPTS_TX_ID=c21b70a719112f76d318abcfee95499a27556ef4a18b1c62bc38e4b9c07ae3a6"
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

    hydraRunFile = [
             "#!/usr/bin/env bash"
            , ""
            , "set -vxe"
            , ""
            , ". hydra-node.environment"
            , ""
            , "./hydra-node \\"
            , "  --node-id  arnaud \\"
            , "  --api-host   0.0.0.0 \\"
            , "  --host  0.0.0.0 \\"
            , "  --port  5001 \\"
            , "  --monitoring-port  6001 \\"
            , "  --persistence-dir  hydra-data \\"
            , "  --hydra-signing-key  keys/arnaud-hydra.sk \\"
            , "  --cardano-signing-key  keys/arnaud.sk \\"
            , "  --ledger-protocol-parameters  protocol-parameters.json \\"
            , "  --network-magic 2 \\"
            , "  --hydra-scripts-tx-id  ${HYDRA_SCRIPTS_TX_ID} \\"
            , "  --node-socket ${SOCKETPATH} \\"
            -- sebastian
            , "  --peer  fk.ncoding.at:5001 \\"
            , "  --cardano-verification-key  keys/sebastian.cardano.vk \\"
            , "  --hydra-verification-key  keys/sebastian.hydra.vk \\"
            -- sasha
            , "  --peer 13.37.150.125:5001 \\"
            , "  --cardano-verification-key keys/sasha.cardano.vk \\"
            , "  --hydra-verification-key keys/sasha.hydra.vk  \\"
            -- franco
            , "  --peer 13.39.83.131:5001 \\"
            , "  --cardano-verification-key keys/franco.cardano.vk \\"
            , "  --hydra-verification-key keys/franco.hydra.vk \\"
            -- daniel
            , "  --peer hydra.horizon-haskell.net:5005 \\"
            , "  --cardano-verification-key keys/daniel.cardano.vk \\"
            , "  --hydra-verification-key keys/daniel.hydra.vk"
       ]
