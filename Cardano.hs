module Cardano where

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess, readProcessEnv)
import System.FilePath ((</>))
import Base(OS)

setupNode :: User -> Property OS
setupNode user =
    propertyList "Cardano node" $
        props
            & Ssh.authorizedKeys user hostContext
            & check
                ( do
                    d <- User.homedir user
                    not <$> doesDirectoryExist (d </> "cardano-configurations")
                )
                (Git.pulled user "https://github.com/input-output-hk/cardano-configurations" "cardano-configurations" Nothing)
            & check
                shouldDownload
                ( cmdProperty
                    "curl"
                    ["-o", archivePath, "-L", "https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-8.0.0-linux.tar.gz"]
                    `changesFileContent` archivePath
                )
            & File.ownerGroup archivePath user userGrp
            & check
                shouldUnpack
                ( cmdProperty
                    "tar"
                    ["xC", "/home/curry", "-f", archivePath]
                    `changesFileContent` "/home/curry/cardano-node"
                )
            & generateTopologyFile
            & File.hasContent "/home/curry/cardano-node.environment" envFile
            & File.hasContent "/etc/systemd/system/cardano-node.service" serviceNode
            & Systemd.enabled "cardano-node"
            & Systemd.started "cardano-node"
  where
    sha256 = "478fb9a9b1f214b22fc076f9c7db93c4b0dd38f1700400eb8ca44fe9e4e7a011"

    archivePath = "/home/curry/cardano-node-8.0.0.tgz"

    shouldUnpack =
        liftPropellor $ do
            dir <- User.homedir user
            hasFile <- doesFileExist (dir </> "cardano-node")
            if not hasFile
                then pure True
                else not . ("8.0.0" `elem`) . words . head . lines <$> readProcessEnv "/home/curry/cardano-node" ["--version"] (Just [("LD_LIBRARY_PATH", dir)])

    shouldDownload = liftPropellor $ do
        hasFile <- doesFileExist archivePath
        if not hasFile
            then pure True
            else (/= sha256) . head . words . head . lines <$> readProcess "sha256sum" [archivePath]

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
        propertyList "Random topology.json" $
            props
                & Apt.installed ["jq", "curl", "coreutils"]
                & check
                    (not <$> doesFileExist "/home/curry/topology.json")
                    (scriptProperty [randomPeers])
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
