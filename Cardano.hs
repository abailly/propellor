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
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess, readProcessEnv)
import System.FilePath ((</>))

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
                    ["-o", archivePath, "-L", "https://github.com/input-output-hk/cardano-node/releases/download/8.1.2/cardano-node-8.1.2-linux.tar.gz"]
                    `changesFileContent` archivePath
                )
            `describe` "Cardano node 8.1.2 archive downloaded"
            & File.ownerGroup archivePath user userGrp
            & check
                shouldUnpack
                ( cmdProperty
                    "tar"
                    ["xC", "/home/curry", "-f", archivePath]
                    `changesFileContent` "/home/curry/cardano-node"
                )
            `describe` "Cardano node 8.1.2 archive unpacked"
            & generateTopologyFile
            & File.hasContent "/home/curry/cardano-node.environment" envFile
            & File.hasContent "/etc/systemd/system/cardano-node.service" serviceNode
            & mithrilSnapshotDownloaded
            & Systemd.enabled "cardano-node"
            & Systemd.restarted "cardano-node"
  where
    sha256 = "35a9116cd7d47f527d3480853aaf8732b7cf1eeacf7a67530bca6a7fd69e50fa"

    shouldUnpack = do
        dir <- User.homedir user
        hasFile <- doesFileExist (dir </> "cardano-node")
        if hasFile
            then
                not
                    . ("8.0.0" `elem`)
                    . words
                    . head
                    . lines
                    <$> readProcessEnv ("/home/curry" </> "cardano-node") ["--version"] (Just [("LD_LIBRARY_PATH", dir)])
            else pure True

    archivePath = "/home/curry/cardano-node-8.1.2.tgz"

    userGrp = Group "curry"

    envFile =
        [ "CONFIG=\"/home/curry/cardano-configurations/network/mainnet/cardano-node/config.json\""
        , "TOPOLOGY=\"/home/curry/topology.json\""
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

    generateTopologyFile =
        propertyList "Cardano topology created" $
            props
                & Apt.installed ["jq", "curl", "coreutils"]
                & check
                    (not <$> doesFileExist "/home/curry/topology.json")
                    (scriptProperty [randomPeers])
                `describe` "Random topology.json generated"
                & File.ownerGroup "/home/curry/topology.json" user userGrp

    randomPeers =
        concat
            [ "curl https://explorer.mainnet.cardano.org/relays/topology.json | "
            , "jq -rc '(.Producers[] | "
            , "{addr:.addr,port:.port,valency:1})' | "
            , "shuf | "
            , "head -20 | "
            , "jq -s '(. | {Producers:.})' > "
            , "/home/curry/topology.json"
            ]

mithrilSnapshotDownloaded ::
    Property
        ( MetaTypes
            '[ 'Targeting 'OSDebian
             , 'Targeting 'OSBuntish
             , 'Targeting 'OSArchLinux
             , 'Targeting 'OSFreeBSD
             ]
        )
mithrilSnapshotDownloaded =
    propertyList "Mithril snapshot downloaded" $
        props
            & check
                (shouldDownload sha256 mithrilPath)
                ( cmdProperty
                    "curl"
                    ["-o", mithrilPath, "-L", "https://github.com/input-output-hk/mithril/releases/download/2331.1/mithril-client_0.3.27+ff06651_amd64.deb"]
                    `changesFileContent` mithrilPath
                )
            `describe` ("Mithril client " <> mithrilClientVersion <> " package downloaded")
            & check
                shouldUnpack
                ( cmdProperty "dpkg" ["--install", mithrilPath]
                    `assume` MadeChange
                    `describe` ("Mithril client " <> mithrilClientVersion <> " package installed")
                )
            & File.containsLines
                "/home/user/mithril-client.env"
                [ "AGGREGATOR_ENDPOINT=" <> aggregatorEndpoint <> "\""
                , "GENESIS_VERIFICATION_KEY=" <> genesisVerificationKey <> "\""
                ]

  where
    aggregatorEndpoint = "https://aggregator.release-mainnet.api.mithril.network/aggregator️"

    genesisVerificationKey = "5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d"

    mithrilSnapshot = "fdd609c5affa627c9b19dfd32c5a370a9e6ba0f930ec50281b5f470fe3c955de"

    sha256 = "fc63de9c6b96185166066acfff4fa94cb329d79f3d8e1fd7adc7490defb20fb6"

    mithrilPath = "mithril-client.deb"

    mithrilClientVersion = "0.3.27+ff06651"

    shouldUnpack =
        not
            . (mithrilClientVersion `elem`)
            . words
            . head
            . lines
            <$> readProcess ("/usr/bin" </> "mithril-client") ["--version"]

shouldDownload :: String -> FilePath -> IO Bool
shouldDownload sha256 archivePath = do
    hasFile <- doesFileExist archivePath
    if not hasFile
        then pure True
        else (/= sha256) . head . words . head . lines <$> readProcess "/usr/bin/sha256sum" [archivePath]
