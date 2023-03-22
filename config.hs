-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import Propellor.Property.Firewall (Chain (..), ConnectionState (..), Proto (..), Rules (..), Table (..), Target (..), rule)
import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess)

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts = [cardano]

-- An example host.
cardano :: Host
cardano =
    host "cardano.hydra.bzh" $
        props
            & osDebian Unstable X86_64
            & Apt.stdSourcesList
            & Apt.unattendedUpgrades
            & Apt.installed ["etckeeper"]
            & Apt.installed ["ssh", "jq", "tmux", "dstat"]
            & Ssh.installed
            & Tor.installed
            & Tor.hiddenServiceAvailable "ssh" (Port 22)
            & User.hasSomePassword (User "root")
            & File.dirExists "/var/www"
            & Cron.runPropellor (Cron.Times "30 * * * *")
            & Systemd.persistentJournal
            & firewall
            & setupNode
            & setupHydraNode

type OS = MetaTypes '[ 'WithInfo, 'Targeting 'OSDebian, 'Targeting 'OSBuntish]

firewall :: Property OS
firewall =
    propertyList "firewall accepts ssh and Cardano relay " $
        props
            & flush INPUT
            & firewallPreamble
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 22))
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 3001))
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 5001))
            & dropEverything

setupHydraNode =
    propertyList "Cardano node" $
        props
            & File.hasContent "/etc/systemd/system/hydra-node.service" serviceFile
            & Systemd.enabled "hydra-node"
            & Systemd.started "hydra-node"
  where
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

setupNode :: Property OS
setupNode =
    propertyList "Cardano node" $
        props
            & User.accountFor user
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

{- |A basic rule to drop every input packet

 This should be used as last clause for a bunch of rules, like:
-}
dropEverything :: Property Linux
dropEverything = rule INPUT Filter DROP Everything

{- |Drop all rules for given chain.

 Useful at start of configuration of firewall rules
-}
flush :: Chain -> Property OS
flush chain =
    property ("flushing all rules for chain " <> show chain) $
        liftPropellor $
            toResult <$> boolSystem "iptables" (map Param ["-F", show chain])

firewallPreamble :: Property (MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish])
firewallPreamble =
    propertyList "standard firewall preamble rules (opens lo and docker0)" $
        props
            & Firewall.installed
            & Firewall.rule INPUT Filter ACCEPT (Ctstate [ESTABLISHED, RELATED])
            & Firewall.rule INPUT Filter ACCEPT (InIFace "lo")

openCommonPorts :: Property Linux
openCommonPorts =
    propertyList "open common operating ports for web" $
        props
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 22))
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 80))
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 443))
