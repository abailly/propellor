-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/
{-# LANGUAGE DataKinds #-}

import Base (OS)
import qualified Cardano
import Propellor
import Propellor.Base (catchMaybeIO, combineModes, hPutStr, liftIO, processTranscript, stderr, (</>))
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Docker as Docker
import qualified Propellor.Property.File as File
import Propellor.Property.Firewall (Chain (..), ConnectionState (..), Proto (..), Rules (..), Table (..), Target (..), rule)
import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Git as Git
import Propellor.Property.LetsEncrypt (AgreeTOS (..), certFile, chainFile, fullChainFile, privKeyFile)
import qualified Propellor.Property.LetsEncrypt as LetsEncrypt
import qualified Propellor.Property.Nginx as Nginx
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Tor as Tor
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesFileExist, readProcess)
import System.Posix (deviceID, fileID, fileMode, fileSize, getFileStatus, modificationTime, ownerExecuteMode, ownerReadMode, ownerWriteMode)

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts = [clermont, cardano]

letsEncryptAgree :: LetsEncrypt.AgreeTOS
letsEncryptAgree = LetsEncrypt.AgreeTOS (Just "me@punkachien.net")

basePackages :: [String]
basePackages =
    [ "etckeeper"
    , "ssh"
    , "jq"
    , "tmux"
    , "dstat"
    , "git"
    , "emacs-nox"
    , "silversearcher-ag"
    , "direnv"
    , "python3-pip"
    , "dstat"
    , "btop"
    , "tcpdump"
    , "dmidecode"
    , "hwinfo"
    , "sqlite3"
    ]

clermont :: Host
clermont =
    host "clermont" $ do
        props
            & osDebian Unstable X86_64
            & alias "www.punkachien.net"
            & Ssh.authorizedKeys root hostContext
            & Ssh.noPasswords
            & Apt.stdSourcesList
            & Apt.unattendedUpgrades
            & Apt.update
            & Apt.installed basePackages
            & installNix
            & File.hasContent "/etc/nix/nix.conf" nixConf
            & Systemd.started "nix-daemon.service"
            & Ssh.installed
            & Systemd.persistentJournal
            & setupUser user
            & Cardano.setup user
            & File.dirExists "/var/www"
            & File.ownerGroup "/var/www" user userGrp
            & Nginx.siteEnabled "www.punkachien.net" punkachien
            `onChange` selfSignedCert "www.punkachien.net"
            `requires` File.hasContent "/etc/nginx/conf.d/connection-upgrade.conf" connectionUpgradeConf
            & Nginx.siteEnabled "jupyter.mithril.network" jupyter
            `onChange` selfSignedCert "jupyter.mithril.network"
            & letsEncryptCertsInstalled letsEncryptAgree ["www.punkachien.net", "jupyter.mithril.network"]
            `onChange` Nginx.reloaded
            & installRust
            & dockerComposeInstalled
            & Docker.installed
  where
    root = User "root"
    user = User "curry"
    userGrp = Group "curry"
    nixGrp = Group "nixbld"
    systemdJournal = Group "systemd-journal"

    dockerComposeInstalled =
        Apt.installed ["docker-compose-plugin"]
            `requires` Docker.installed
            `requires` Apt.setSourcesListD ["deb [arch=amd64] https://download.docker.com/linux/debian bookworm stable"] "docker"
            `requires` Apt.trustsKey dockerKey

    -- from https://futurestud.io/tutorials/nginx-how-to-fix-unknown-connection_upgrade-variable
    connectionUpgradeConf =
        [ "map $http_upgrade $connection_upgrade {  "
        , "    default upgrade;"
        , "    ''      close;"
        , "}"
        ]

    setupUser u =
        propertyList ("Configured user " <> show u) $
            props
                & User.accountFor u
                & Ssh.authorizedKey user ""
                & Ssh.authorizedKeys user hostContext
                & Ssh.userKeyAt
                    Nothing
                    user
                    hostContext
                    ( SshEd25519
                    , "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIERjBICdoL0S4dU+HgevTutHF0QajK/qEN1iHKgeU7+T Remote curry user's key"
                    )
                & User.hasGroup u nixGrp
                & User.hasGroup u systemdJournal
                & Git.cloned user "git@github.com:abailly-iohk/dotfiles" "/home/curry/dotfiles" Nothing
                & File.hasContent "/home/curry/sensei/hooks/update" senseiUpdateHook
                `requires` stackInstalled
                `requires` Git.bareRepo "sensei" user Git.NotShared
                `onChange` ( "/home/curry/sensei/hooks/update"
                                `File.mode` combineModes [ownerReadMode, ownerWriteMode, ownerExecuteMode]
                           )
                `onChange` File.ownerGroup "/home/curry/sensei/hooks/update" user userGrp
                & Sudo.enabledFor u
                & File.hasPrivContent "/home/curry/.config/sensei/client.json" anyContext
                `requires` File.dirExists "/home/curry/.config/sensei/"
                `requires` File.applyPath "/home/curry/.config" "sensei/client.json" (\f -> File.ownerGroup f u userGrp)

    senseiUpdateHook =
        [ "#!/bin/sh"
        , "echo \"Updating $1 from $2 to $3\""
        ]

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

    installRust =
        check
            doesNotHaveRust
            ( userScriptProperty
                user
                [ "curl -sSf https://sh.rustup.rs | sudo RUSTUP_HOME=/opt/rust CARGO_HOME=/opt/rust sh -s -- --no-modify-path -y"
                , "echo 'export RUSTUP_HOME=/opt/rust' | sudo tee -a /etc/profile.d/rust.sh"
                , "echo 'export PATH=$PATH:/opt/rust/bin' | sudo tee -a /etc/profile.d/rust.sh"
                ]
            )
            `requires` Apt.installed ["gcc", "build-essential", "m4", "pkgconf", "libssl-dev"]
            `describe` "Rustup toolchain installed"

    doesNotHaveRust =
        not <$> doesFileExist "/opt/rust/bin/rustc"

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

    stackInstalled =
        check
            shouldInstallStack
            ( scriptProperty
                [ "curl -o install-stack -sSL https://get.haskellstack.org/"
                , "/bin/sh install-stack -d /usr/local/bin"
                ]
            )
            `describe` "Stack 2.11.1 installed"

    shouldInstallStack = do
        hasStack <- doesFileExist "/usr/local/bin/stack"
        if hasStack
            then not . ("2.11.1," `elem`) . words <$> readProcess "/usr/local/bin/stack" ["--version"]
            else pure True

    punkachien =
        [ "server {"
        , "    listen 80;"
        , "    listen [::]:80;"
        , "    "
        , "    root /var/www/punkachien.net/public_html;"
        , "    index index.html index.htm index.nginx-debian.html;"
        , "    "
        , "    server_name www.punkachien.net punkachien.net;"
        , "    "
        , "    listen 443 ssl; # managed by Certbot"
        , ""
        , "    # RSA certificate"
        , "    ssl_certificate /etc/letsencrypt/live/www.punkachien.net/fullchain.pem; # managed by Certbot"
        , "    ssl_certificate_key /etc/letsencrypt/live/www.punkachien.net/privkey.pem; # managed by Certbot"
        , ""
        , "    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot"
        , ""
        , "    # Redirect non-https traffic to https"
        , "    if ($scheme != \"https\") {"
        , "        return 301 https://$host$request_uri;"
        , "    } # managed by Certbot"
        , ""
        , "    location / {"
        , "            try_files $uri $uri/ =404;"
        , "    }"
        , "}"
        ]

    jupyter =
        [ "server {"
        , "    listen 80;"
        , "    listen [::]:80;"
        , "    server_name jupyter.mithril.network;"
        , "    location ~ /.well-known {"
        , "        allow all;"
        , "        root /var/www/jupyter.mithril.net/public_html;"
        , "    }"
        , "    location / {"
        , "            rewrite ^ https://$host$request_uri? permanent;"
        , "    }"
        , "}"
        , ""
        , "server {"
        , "    server_name jupyter.mithril.network;"
        , "    "
        , "    listen 443 ssl; # managed by Certbot"
        , ""
        , "    # RSA certificate"
        , "    ssl_certificate /etc/letsencrypt/live/jupyter.mithril.network/fullchain.pem; # managed by Certbot"
        , "    ssl_certificate_key /etc/letsencrypt/live/jupyter.mithril.network/privkey.pem; # managed by Certbot"
        , ""
        , "    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot"
        , ""
        , "    location / {"
        , "        proxy_pass http://127.0.0.1:8888;"
        , "        proxy_set_header X-Real-IP $remote_addr;"
        , "        proxy_set_header Host $host;"
        , "        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;"
        , ""
        , "        # websocket headers"
        , "        proxy_http_version 1.1;"
        , "        proxy_set_header Upgrade $http_upgrade;"
        , "        proxy_set_header Connection $connection_upgrade;"
        , "        proxy_set_header X-Scheme $scheme;"
        , ""
        , "        proxy_buffering off;"
        , "    }"
        , "}"
        ]

letsEncryptCertsInstalled :: AgreeTOS -> [String] -> Property DebianLike
letsEncryptCertsInstalled (AgreeTOS memail) domains =
    prop `requires` Apt.installed ["certbot", "python3-certbot-nginx"]
  where
    prop :: Property UnixLike
    prop = property desc $ do
        startstats <- liftIO getstats
        (transcript, ok) <-
            liftIO $
                processTranscript "letsencrypt" params Nothing
        if ok
            then do
                endstats <- liftIO getstats
                if startstats /= endstats
                    then return MadeChange
                    else return NoChange
            else do
                liftIO $ hPutStr stderr transcript
                return FailedChange

    desc = "letsencrypt " ++ unwords domains
    params =
        [ "--nginx"
        , "--agree-tos"
        , case memail of
            Just email -> "--email=" ++ email
            Nothing -> "--register-unsafely-without-email"
        , "--noninteractive"
        , "--keep-until-expiring"
        , -- The list of domains may be changed, adding more, so
          -- always request expansion.
          "--expand"
        ]
            ++ map (\d -> "--domain=" ++ d) domains

    getstats = mapM statcertfiles domains
    statcertfiles d =
        mapM
            statfile
            [ certFile d
            , privKeyFile d
            , chainFile d
            , fullChainFile d
            ]
    statfile f = catchMaybeIO $ do
        s <- getFileStatus f
        return (fileID s, deviceID s, fileMode s, fileSize s, modificationTime s)

selfSignedCert :: FilePath -> Property DebianLike
selfSignedCert domain =
    selfSignedGenerated
        `requires` File.dirExists ("/etc/letsencrypt/live" </> domain)
  where
    selfSignedGenerated :: Property DebianLike
    selfSignedGenerated = property desc $ do
        hasCert <- liftIO $ doesFileExist certFile
        if hasCert
            then pure NoChange
            else genSelfSignedCert

    desc = "Self-signed cert for " <> domain
    genSelfSignedCert = do
        (transcript, ok) <- liftIO $ processTranscript "openssl" params Nothing
        if ok
            then do
                hasCert <- liftIO $ doesFileExist certFile
                if hasCert
                    then return MadeChange
                    else return FailedChange
            else do
                liftIO $ hPutStr stderr transcript
                return FailedChange

    certFile = "/etc/letsencrypt/live" </> domain </> "fullchain.pem"
    keyFile = "/etc/letsencrypt/live" </> domain </> "privkey.pem"
    params =
        [ "req"
        , "-x509"
        , "-nodes"
        , "-days"
        , "365"
        , "-newkey"
        , "rsa:4096"
        , "-subj"
        , "/C=FR/ST=France/L=Paris/CN=" <> domain
        , "-keyout"
        , keyFile
        , "-out"
        , certFile
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
            & Cardano.setup user
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
            & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 5002))
            & dropEverything

setupHydraNode :: Property (MetaTypes '[ 'Targeting 'OSDebian, 'Targeting 'OSBuntish, 'Targeting 'OSArchLinux])
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

{- | A basic rule to drop every input packet

 This should be used as last clause for a bunch of rules, like:
-}
dropEverything :: Property Linux
dropEverything = rule INPUT Filter DROP Everything

{- | Drop all rules for given chain.

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

dockerKey :: Apt.AptKey
dockerKey =
    Apt.AptKey "docker" $
        unlines
            [ "-----BEGIN PGP PUBLIC KEY BLOCK-----"
            , ""
            , "mQINBFit2ioBEADhWpZ8/wvZ6hUTiXOwQHXMAlaFHcPH9hAtr4F1y2+OYdbtMuth"
            , "lqqwp028AqyY+PRfVMtSYMbjuQuu5byyKR01BbqYhuS3jtqQmljZ/bJvXqnmiVXh"
            , "38UuLa+z077PxyxQhu5BbqntTPQMfiyqEiU+BKbq2WmANUKQf+1AmZY/IruOXbnq"
            , "L4C1+gJ8vfmXQt99npCaxEjaNRVYfOS8QcixNzHUYnb6emjlANyEVlZzeqo7XKl7"
            , "UrwV5inawTSzWNvtjEjj4nJL8NsLwscpLPQUhTQ+7BbQXAwAmeHCUTQIvvWXqw0N"
            , "cmhh4HgeQscQHYgOJjjDVfoY5MucvglbIgCqfzAHW9jxmRL4qbMZj+b1XoePEtht"
            , "ku4bIQN1X5P07fNWzlgaRL5Z4POXDDZTlIQ/El58j9kp4bnWRCJW0lya+f8ocodo"
            , "vZZ+Doi+fy4D5ZGrL4XEcIQP/Lv5uFyf+kQtl/94VFYVJOleAv8W92KdgDkhTcTD"
            , "G7c0tIkVEKNUq48b3aQ64NOZQW7fVjfoKwEZdOqPE72Pa45jrZzvUFxSpdiNk2tZ"
            , "XYukHjlxxEgBdC/J3cMMNRE1F4NCA3ApfV1Y7/hTeOnmDuDYwr9/obA8t016Yljj"
            , "q5rdkywPf4JF8mXUW5eCN1vAFHxeg9ZWemhBtQmGxXnw9M+z6hWwc6ahmwARAQAB"
            , "tCtEb2NrZXIgUmVsZWFzZSAoQ0UgZGViKSA8ZG9ja2VyQGRvY2tlci5jb20+iQI3"
            , "BBMBCgAhBQJYrefAAhsvBQsJCAcDBRUKCQgLBRYCAwEAAh4BAheAAAoJEI2BgDwO"
            , "v82IsskP/iQZo68flDQmNvn8X5XTd6RRaUH33kXYXquT6NkHJciS7E2gTJmqvMqd"
            , "tI4mNYHCSEYxI5qrcYV5YqX9P6+Ko+vozo4nseUQLPH/ATQ4qL0Zok+1jkag3Lgk"
            , "jonyUf9bwtWxFp05HC3GMHPhhcUSexCxQLQvnFWXD2sWLKivHp2fT8QbRGeZ+d3m"
            , "6fqcd5Fu7pxsqm0EUDK5NL+nPIgYhN+auTrhgzhK1CShfGccM/wfRlei9Utz6p9P"
            , "XRKIlWnXtT4qNGZNTN0tR+NLG/6Bqd8OYBaFAUcue/w1VW6JQ2VGYZHnZu9S8LMc"
            , "FYBa5Ig9PxwGQOgq6RDKDbV+PqTQT5EFMeR1mrjckk4DQJjbxeMZbiNMG5kGECA8"
            , "g383P3elhn03WGbEEa4MNc3Z4+7c236QI3xWJfNPdUbXRaAwhy/6rTSFbzwKB0Jm"
            , "ebwzQfwjQY6f55MiI/RqDCyuPj3r3jyVRkK86pQKBAJwFHyqj9KaKXMZjfVnowLh"
            , "9svIGfNbGHpucATqREvUHuQbNnqkCx8VVhtYkhDb9fEP2xBu5VvHbR+3nfVhMut5"
            , "G34Ct5RS7Jt6LIfFdtcn8CaSas/l1HbiGeRgc70X/9aYx/V/CEJv0lIe8gP6uDoW"
            , "FPIZ7d6vH+Vro6xuWEGiuMaiznap2KhZmpkgfupyFmplh0s6knymuQINBFit2ioB"
            , "EADneL9S9m4vhU3blaRjVUUyJ7b/qTjcSylvCH5XUE6R2k+ckEZjfAMZPLpO+/tF"
            , "M2JIJMD4SifKuS3xck9KtZGCufGmcwiLQRzeHF7vJUKrLD5RTkNi23ydvWZgPjtx"
            , "Q+DTT1Zcn7BrQFY6FgnRoUVIxwtdw1bMY/89rsFgS5wwuMESd3Q2RYgb7EOFOpnu"
            , "w6da7WakWf4IhnF5nsNYGDVaIHzpiqCl+uTbf1epCjrOlIzkZ3Z3Yk5CM/TiFzPk"
            , "z2lLz89cpD8U+NtCsfagWWfjd2U3jDapgH+7nQnCEWpROtzaKHG6lA3pXdix5zG8"
            , "eRc6/0IbUSWvfjKxLLPfNeCS2pCL3IeEI5nothEEYdQH6szpLog79xB9dVnJyKJb"
            , "VfxXnseoYqVrRz2VVbUI5Blwm6B40E3eGVfUQWiux54DspyVMMk41Mx7QJ3iynIa"
            , "1N4ZAqVMAEruyXTRTxc9XW0tYhDMA/1GYvz0EmFpm8LzTHA6sFVtPm/ZlNCX6P1X"
            , "zJwrv7DSQKD6GGlBQUX+OeEJ8tTkkf8QTJSPUdh8P8YxDFS5EOGAvhhpMBYD42kQ"
            , "pqXjEC+XcycTvGI7impgv9PDY1RCC1zkBjKPa120rNhv/hkVk/YhuGoajoHyy4h7"
            , "ZQopdcMtpN2dgmhEegny9JCSwxfQmQ0zK0g7m6SHiKMwjwARAQABiQQ+BBgBCAAJ"
            , "BQJYrdoqAhsCAikJEI2BgDwOv82IwV0gBBkBCAAGBQJYrdoqAAoJEH6gqcPyc/zY"
            , "1WAP/2wJ+R0gE6qsce3rjaIz58PJmc8goKrir5hnElWhPgbq7cYIsW5qiFyLhkdp"
            , "YcMmhD9mRiPpQn6Ya2w3e3B8zfIVKipbMBnke/ytZ9M7qHmDCcjoiSmwEXN3wKYI"
            , "mD9VHONsl/CG1rU9Isw1jtB5g1YxuBA7M/m36XN6x2u+NtNMDB9P56yc4gfsZVES"
            , "KA9v+yY2/l45L8d/WUkUi0YXomn6hyBGI7JrBLq0CX37GEYP6O9rrKipfz73XfO7"
            , "JIGzOKZlljb/D9RX/g7nRbCn+3EtH7xnk+TK/50euEKw8SMUg147sJTcpQmv6UzZ"
            , "cM4JgL0HbHVCojV4C/plELwMddALOFeYQzTif6sMRPf+3DSj8frbInjChC3yOLy0"
            , "6br92KFom17EIj2CAcoeq7UPhi2oouYBwPxh5ytdehJkoo+sN7RIWua6P2WSmon5"
            , "U888cSylXC0+ADFdgLX9K2zrDVYUG1vo8CX0vzxFBaHwN6Px26fhIT1/hYUHQR1z"
            , "VfNDcyQmXqkOnZvvoMfz/Q0s9BhFJ/zU6AgQbIZE/hm1spsfgvtsD1frZfygXJ9f"
            , "irP+MSAI80xHSf91qSRZOj4Pl3ZJNbq4yYxv0b1pkMqeGdjdCYhLU+LZ4wbQmpCk"
            , "SVe2prlLureigXtmZfkqevRz7FrIZiu9ky8wnCAPwC7/zmS18rgP/17bOtL4/iIz"
            , "QhxAAoAMWVrGyJivSkjhSGx1uCojsWfsTAm11P7jsruIL61ZzMUVE2aM3Pmj5G+W"
            , "9AcZ58Em+1WsVnAXdUR//bMmhyr8wL/G1YO1V3JEJTRdxsSxdYa4deGBBY/Adpsw"
            , "24jxhOJR+lsJpqIUeb999+R8euDhRHG9eFO7DRu6weatUJ6suupoDTRWtr/4yGqe"
            , "dKxV3qQhNLSnaAzqW/1nA3iUB4k7kCaKZxhdhDbClf9P37qaRW467BLCVO/coL3y"
            , "Vm50dwdrNtKpMBh3ZpbB1uJvgi9mXtyBOMJ3v8RZeDzFiG8HdCtg9RvIt/AIFoHR"
            , "H3S+U79NT6i0KPzLImDfs8T7RlpyuMc4Ufs8ggyg9v3Ae6cN3eQyxcK3w0cbBwsh"
            , "/nQNfsA6uu+9H7NhbehBMhYnpNZyrHzCmzyXkauwRAqoCbGCNykTRwsur9gS41TQ"
            , "M8ssD1jFheOJf3hODnkKU+HKjvMROl1DK7zdmLdNzA1cvtZH/nCC9KPj1z8QC47S"
            , "xx+dTZSx4ONAhwbS/LN3PoKtn8LPjY9NP9uDWI+TWYquS2U+KHDrBDlsgozDbs/O"
            , "jCxcpDzNmXpWQHEtHU7649OXHP7UeNST1mCUCH5qdank0V1iejF6/CfTFU4MfcrG"
            , "YT90qFF93M3v01BbxP+EIY2/9tiIPbrd"
            , "=0YYh"
            , "-----END PGP PUBLIC KEY BLOCK-----"
            ]
