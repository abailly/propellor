-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/
{-# LANGUAGE DataKinds #-}

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess)

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
    [ cardano
    ]

-- An example host.
cardano :: Host
cardano =
    host "cardano.hydra.bzh" $
        props
            & osDebian Unstable X86_64
            & Apt.stdSourcesList
            & Apt.unattendedUpgrades
            & Apt.installed ["etckeeper"]
            & Apt.installed ["ssh", "jq", "tmux"]
            & User.hasSomePassword (User "root")
            & File.dirExists "/var/www"
            & Cron.runPropellor (Cron.Times "30 * * * *")
            & Systemd.persistentJournal
            & setupNode

setupNode :: Property (MetaTypes '[ 'WithInfo, 'Targeting 'OSDebian, 'Targeting 'OSBuntish])
setupNode =
    propertyList "Cardano node" $
        props
            & User.accountFor user
            & Ssh.installed
            & Ssh.authorizedKeys user hostContext
            & check
                (not <$> doesDirectoryExist "/home/curry/cardano-configurations")
                (Git.pulled user "https://github.com/input-output-hk/cardano-configurations" "cardano-configurations" Nothing)
            & check
                shouldDownload
                ( cmdProperty
                    "curl"
                    ["-o", archivePath, "-L", "https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.5-linux.tar.gz"]
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
    sha256 = "bb9e9c3700ebdef4de3e34e5087a79dc30d27ca3c1c66af25957f9205dfe05aa"

    archivePath = "/home/curry/cardano-node-1.35.5.tgz"

    shouldUnpack =
        liftPropellor $ do
            hasFile <- doesFileExist "/home/curry/cardano-node"
            if not hasFile
                then pure True
                else not . ("1.35.5" `elem`) . words . head . lines <$> readProcess "/home/curry/cardano-node" ["--version"]

    shouldDownload = liftPropellor $ do
        hasFile <- doesFileExist archivePath
        if not hasFile
            then pure True
            else (/= sha256) . head . words . head . lines <$> readProcess "sha256sum" [archivePath]

    user = User "curry"

    userGrp = Group "curry"

    envFile =
        [ "CONFIG=\"/home/curry/cardano-configurations/network/mainnet/cardano-node/config.json\""
        , "TOPOLOGY=\"/home/curry/topology.json\""
        , "DBPATH=\"./db/\""
        , "SOCKETPATH=\"./node.socket\""
        , "HOSTADDR=\"0.0.0.0\""
        , "PORT=\"3000\""
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
