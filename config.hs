-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Base (OS)
import Cardano (setupNode)
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import Propellor.Property.Firewall (Chain (..), ConnectionState (..), Proto (..), Rules (..), Table (..), Target (..), rule)
import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Nginx as Nginx
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (readProcess)

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts = [clermont, cardano]

clermont :: Host
clermont =
    host "clermont.home" $
        props
            & osDebian Unstable X86_64
            & Apt.stdSourcesList
            & Apt.unattendedUpgrades
            & Apt.installed ["etckeeper"]
            & Apt.installed ["ssh", "jq", "tmux", "dstat", "git", "emacs-nox"]
            & installNix
            & File.hasContent "/etc/nix/nix.conf" nixConf
            & Systemd.started "nix-daemon.service"
            & Ssh.installed
            & Systemd.persistentJournal
            & User.accountFor user
            & User.hasGroup user nixGrp
            & User.hasGroup user systemdJournal
            & Ssh.authorizedKey user ""
            & setupNode user
            & File.dirExists "/var/www"
            & File.ownerGroup "/var/www" user userGrp
            & Nginx.siteEnabled "www.punkachien.net" punkachien
  where
    user = User "curry"
    userGrp = Group "curry"
    nixGrp = Group "nixbld"
    systemdJournal = Group "systemd-journal"
    nixConf =
        [ "max-jobs = 6"
        , "cores = 0"
        , "trusted-users = root curry"
        , "keep-derivations = true"
        , "keep-outputs = true"
        , "experimental-features = nix-command flakes"
        , "substituters = https://cache.nixos.org https://hydra.iohk.io https://iohk.cachix.org https://cache.iog.io https://cardano-scaling.cachix.org https://cache.zw3rk.com"
        , "trusted-public-keys = iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= cardano-scaling.cachix.org-1:RKvHKhGs/b6CBDqzKbDk0Rv6sod2kPSXLwPzcUQg9lY= loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
        ]
    shouldInstallNix =
        not . ("2.15.0" `elem`) . words <$> readProcess "/nix/var/nix/profiles/default/bin/nix" ["--version"]

    installNix =
        check
            shouldInstallNix
            ( scriptProperty
                [ "curl -o install-nix-2.15.0 https://releases.nixos.org/nix/nix-2.15.0/install"
                , "chmod +x ./install-nix-2.15.0"
                , "./install-nix-2.15.0 --daemon < /dev/null"
                ]
            )
            `describe` "Nix 2.15.0 installed"

    punkachien =
        [
          "server {",
          "    listen 80 default_server;",
          "    listen [::]:80 default_server;",
          "    ",
          "    root /var/www/punkachien.net/public_html;",
          "    index index.html index.htm index.nginx-debian.html;",
          "    ",
          "    server_name www.punkachien.net punkachien.net;",
          "    ",
          "    location / {",
          "            try_files $uri $uri/ =404;",
          "    }",
          "}"
        ]

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
            & setupNode user
            & setupHydraNode
  where
    user = User "curry"

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
            & File.hasContent "/home/curry/hydra-node.environment" envFile
            & File.hasContent "/etc/systemd/system/hydra-node.service" serviceFile
            & Systemd.enabled "hydra-node"
            & Systemd.started "hydra-node"
  where
    envFile =
        [ "SOCKETPATH=/home/curry/node.socket"
        , "HYDRA_SCRIPTS_TX_ID=4a4f3e25887b40f1575a4b53815996145c994559bac1b5d85f7de0f82b8f4ed5"
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
