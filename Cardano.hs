{-# LANGUAGE DataKinds #-}

module Cardano where

import Base (OSNoInfo)
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess, readProcessEnv, writeReadProcessEnv)
import System.FilePath ((<.>), (</>))
import System.IO(hPutStr)

setup :: User -> Property OSNoInfo
setup user =
    propertyList "Cardano node" $
        props
            & check
                ( do
                    d <- User.homedir user
                    not <$> doesDirectoryExist (d </> "cardano-configurations")
                )
                (Git.pulled user "https://github.com/input-output-hk/cardano-configurations" "cardano-configurations" Nothing)
               `describe` "Cardano configurations pulled"
            & check
                (shouldDownload sha256 archivePath)
                ( cmdProperty
                    "curl"
                    ["-o", archivePath, "-L", "https://github.com/input-output-hk/cardano-node/releases/download/8.7.3/cardano-node-8.7.3-linux.tar.gz"]
                    `changesFileContent` archivePath
                )
            `describe` "Cardano node 8.7.3 archive downloaded"
            & File.ownerGroup archivePath user userGrp
            & check
                shouldUnpack
                ( cmdProperty
                    "tar"
                    ["xC", "/home/curry", "-f", archivePath]
                    `changesFileContent` "/home/curry/cardano-node"
                )
            `describe` "Cardano node 8.7.3 archive unpacked"
            & File.hasContent "/home/curry/cardano-node.environment" envFile
            & File.hasContent "/etc/systemd/system/cardano-node.service" serviceNode
            & Apt.removed ["mithril-client"]
            & mithrilSnapshotDownloaded user userGrp
            & Systemd.enabled "cardano-node"
            & Systemd.restarted "cardano-node"
  where
    sha256 = "fea39964590885eb2bcf7bd8e78cb11f8bde4b29bb10ca743f41c497cfd9f327"

    shouldUnpack = do
        dir <- User.homedir user
        hasFile <- doesFileExist (dir </> "cardano-node")
        if hasFile
            then
                not
                    . ("8.7.3" `elem`)
                    . words
                    . head
                    . lines
                    <$> readProcessEnv (dir </> "cardano-node") ["--version"] (Just [("LD_LIBRARY_PATH", dir)])
            else pure True

    archivePath = "/home/curry/cardano-node-8.7.3.tgz"

    userGrp = Group "curry"

    envFile =
        [ "CONFIG=\"/home/curry/cardano-configurations/network/mainnet/cardano-node/config.json\""
        , "TOPOLOGY=\"/home/curry/cardano-configurations/network/mainnet/cardano-node/topology.json\""
        , "DBPATH=\"./db/\""
        , "SOCKETPATH=\"./node.socket\""
        , "HOSTADDR=\"0.0.0.0\""
        , "PORT=\"3001\""
        , "LD_LIBRARY_PATH=\"/home/curry\""
        ]

    serviceNode =
        [ "[Unit]"
        , "Description=Cardano node"
        , "After=multi-user.target"
        , ""
        , "[Service]"
        , "Type=simple"
        , "EnvironmentFile=/home/curry/cardano-node.environment"
        , "ExecStart=/home/curry/cardano-node run --config $CONFIG --topology $TOPOLOGY --database-path $DBPATH --socket-path $SOCKETPATH --host-addr $HOSTADDR --port $PORT"
        , "KillSignal = SIGINT"
        , "RestartKillSignal = SIGINT"
        , "StandardOutput=journal"
        , "StandardError=journal"
        , "SyslogIdentifier=cardano-node"
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

mithrilSnapshotDownloaded ::
    User ->
    Group ->
    Property
        ( MetaTypes
            '[ 'Targeting 'OSDebian
             , 'Targeting 'OSBuntish
             , 'Targeting 'OSArchLinux
             ]
        )
mithrilSnapshotDownloaded user userGrp =
    propertyList "Mithril snapshot downloaded" $
        props
            & check
                (shouldDownload archiveSha256 mithrilPath)
                ( cmdProperty
                    "curl"
                    ["-o", mithrilPath, "-L", "https://github.com/input-output-hk/mithril/releases/download/2403.1/mithril-client-cli_0.5.17+254d266-1_amd64.deb"]
                    `changesFileContent` mithrilPath
                )
            `describe` ("Mithril client " <> mithrilClientVersion <> " package downloaded")
            & check
                shouldUnpack
                ( cmdProperty "dpkg" ["--install", mithrilPath]
                    `assume` MadeChange
                    `describe` ("Mithril client " <> mithrilClientVersion <> " package installed")
                )
            & File.hasContent
                "/home/curry/mithril-client.environment"
                [ "export AGGREGATOR_ENDPOINT=\"" <> aggregatorEndpoint <> "\""
                , "export GENESIS_VERIFICATION_KEY=\"" <> genesisVerificationKey <> "\""
                ]
            & File.ownerGroup "/home/curry/mithril-client.environment" user userGrp
            & check
                shouldDownloadSnapshot
                ( userScriptProperty
                    user
                    [ ". ./mithril-client.environment"
                    , "rm -fr db"
                    , "mithril-client snapshot download " <> mithrilSnapshot
                    ]
                    `assume` MadeChange
                    `describe` ("Install Mithril snapshot " <> mithrilSnapshot)
                    `requires` Systemd.stopped "cardano-node"
                )
  where
    aggregatorEndpoint = "https://aggregator.release-mainnet.api.mithril.network/aggregator"

    genesisVerificationKey = "5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d"

    mithrilSnapshot = "177f9d9e3c541cfe255e37faaa2b4a4b340ee6965c46f30e1e0a6c8ac79f6dcd"

    archiveSha256 = "dde2030d987b547e701c57693112d4a14c7676744a8d7bc3dd5ba65a905e8556"

    mithrilPath = "/root/mithril-client.deb"

    mithrilClientVersion = "0.5.17+254d266"

    shouldUnpack = do
      let exe = "/usr/bin/mithril-client"
      hasExe <- doesFileExist exe
      if hasExe
        then
          not
            . (mithrilClientVersion `elem`)
            . words
            . head
            . lines
            <$> readProcess "/usr/bin/mithril-client" ["--version"]
        else pure True

    shouldDownloadSnapshot = do
        dir <- User.homedir user
        let mithrilEnv =  [ ("AGGREGATOR_ENDPOINT", aggregatorEndpoint)
                          , ("GENESIS_VERIFICATION_KEY", genesisVerificationKey)
                          ]

        snapshotJson <- readProcessEnv "/usr/bin/mithril-client"  [ "snapshot", "show", mithrilSnapshot , "--json" ] (Just mithrilEnv)
        lastImmutableFile <- writeReadProcessEnv "jq" [ ".beacon.immutable_file_number" ] Nothing (Just $ \ hdl -> hPutStr hdl snapshotJson) Nothing
        not <$> doesFileExist (dir </> "db" </> "immutable" </> lastImmutableFile <.> "chunk")

shouldDownload :: String -> FilePath -> IO Bool
shouldDownload sha256 archivePath = do
    hasFile <- doesFileExist archivePath
    if not hasFile
        then pure True
        else (/= sha256) . head . words . head . lines <$> readProcess "/usr/bin/sha256sum" [archivePath]
