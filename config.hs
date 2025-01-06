-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Base (OS)
import Cardano (CardanoNetwork (..))
import qualified Cardano
import qualified Hydra
import Propellor
import Propellor.Base (combineModes, liftIO, withPrivData, (</>))
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.Docker as Docker
import Propellor.Property.File (FileWriteMode (ProtectedWrite))
import qualified Propellor.Property.File as File
import Propellor.Property.Firewall (Chain (..), ConnectionState (..), Proto (..), Rules (..), Table (..), Target (..), rule)
import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Nginx as Nginx
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Tor as Tor
import Propellor.Property.User (homedir)
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess)
import qualified Radicle
import System.Posix (ownerExecuteMode, ownerReadMode, ownerWriteMode)
import User (commonUserSetup)
import Web (httpsWebSite)
import qualified Wireguard

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts = [clermont, cardano]

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
  , "hdparm"
  , "fontconfig"
  , "autoconf"
  , "automake"
  , "libtool"
  ]

clermont :: Host
clermont =
  host "clermont" $ do
    props
      & osDebian Unstable X86_64
      & alias "www.punkachien.net"
      & alias "clermont.lan"
      & Ssh.authorizedKeys root hostContext
      & Ssh.noPasswords
      & Apt.stdSourcesList
      & Apt.unattendedUpgrades
      & Apt.update
      & Apt.installed (basePackages <> ["podman", "catatonit", "yaml2json", "nodejs", "npm"])
      & installNix
      & File.hasContent "/etc/nix/nix.conf" nixConf
      & Systemd.started "nix-daemon.service"
      & Tor.installed
      & Ssh.installed
      & Systemd.persistentJournal
      & setupUser user
      & Cardano.setup user Preprod
      & File.dirExists "/var/www"
      & File.ownerGroup "/var/www" user userGrp
      & cgitInstalled
      & httpsWebSite punkachienNet punkachien "me@punkachien.net"
      & httpsWebSite pacificWarNet pacificWarConfig "contact@pankzsoft.net"
      & httpsWebSite gitPankzsoftNet cgit "contact@pankzsoft.net"
      & httpsWebSite "sensei.pankzsoft.net" senseiWebConfig "contact@pankzsoft.net"
      & senseiServerInstalled
      ! Nginx.siteEnabled "git.punkachien.net" []
      & installRust
      & haskellInstalled
      & dockerComposeInstalled
      & Docker.installed
      & Cron.runPropellor (Cron.Times "30 * * * *")
      & Radicle.radicleInstalledFor user
      & Wireguard.clientInstalled
        (Wireguard.WgPublicKey "G+8Gq0jVZ6h9qJ188ycHY5X61FhJ7jMEC7ptdp7dwV0=")
        (Wireguard.Endpoint "95.217.84.233" 51820)
      & firewall
 where
  root = User "root"
  user = User "curry"
  userGrp = Group "curry"
  nixGrp = Group "nixbld"
  dockerGrp = Group "docker"
  punkachienNet = "www.punkachien.net"
  gitPankzsoftNet = "git.pankzsoft.net"
  pacificWarNet = "pacific-war.pankzsoft.net"

  pacificWarConfig =
    [ "server {"
    , "    listen 80;"
    , "    listen [::]:80;"
    , "    "
    , "    root /var/www/pacific-war.pankzsoft.net/public_html;"
    , "    index index.html index.htm index.nginx-debian.html;"
    , "    "
    , "    server_name pacific-war.pankzsoft.net;"
    , "    "
    , "    listen 443 ssl; # managed by Certbot"
    , ""
    , "    # RSA certificate"
    , "    ssl_certificate /etc/letsencrypt/live/pacific-war.pankzsoft.net/fullchain.pem; # managed by Certbot"
    , "    ssl_certificate_key /etc/letsencrypt/live/pacific-war.pankzsoft.net/privkey.pem; # managed by Certbot"
    , ""
    , "    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot"
    , ""
    , "    # Redirect non-https traffic to https"
    , "    if ($scheme != \"https\") {"
    , "        return 301 https://$host$request_uri;"
    , "    } # managed by Certbot"
    , ""
    , "       location / {"
    , "           proxy_pass http://127.0.0.1:8000;"
    , "           proxy_set_header X-Real-IP $remote_addr;"
    , "           proxy_set_header X-Forwarded-Proto $scheme;"
    , "       }"
    , "}"
    ]

  dockerComposeInstalled =
    Apt.installed ["docker-compose-plugin"]
      `requires` Docker.installed
      `requires` Apt.setSourcesListD ["deb [arch=amd64] https://download.docker.com/linux/debian bookworm stable"] "docker"
      `requires` Apt.trustsKey dockerKey

  setupUser u =
    propertyList ("Configured hacking " <> show u) $
      props
        & commonUserSetup u
        & User.hasGroup u nixGrp
        & User.hasGroup u dockerGrp
        & Git.cloned user "git@github.com:abailly-iohk/dotfiles" "/home/curry/dotfiles" Nothing
        & File.hasContent "/home/curry/sensei/hooks/update" senseiUpdateHook
          `requires` stackInstalled
          `requires` Git.bareRepo "sensei" user Git.NotShared
          `onChange` ( "/home/curry/sensei/hooks/update"
                        `File.mode` combineModes [ownerReadMode, ownerWriteMode, ownerExecuteMode]
                     )
          `onChange` File.ownerGroup "/home/curry/sensei/hooks/update" user userGrp
        & File.hasPrivContent "/home/curry/.config/sensei/client.json" anyContext
          `requires` File.dirExists "/home/curry/.config/sensei/"
          `requires` File.applyPath "/home/curry/.config" "sensei/client.json" (\f -> File.ownerGroup f u userGrp)

  senseiUpdateHook =
    [ "#!/usr/bin/env bash"
    , "# A script to build references"
    , ""
    , "branch=$1"
    , "old_commit=$2"
    , "new_commit=$3"
    , ""
    , "if expr \"${branch}\" : '.*/tags/.*' > /dev/null 2>&1; then"
    , "  echo \"got tag ${branch}, not building\""
    , "  exit 0"
    , "fi"
    , ""
    , "echo \"building $branch at $new_commit\""
    , ""
    , "build_dir=\"/tmp/$branch\""
    , ""
    , "[[ -d \"$build_dir\" ]] ||  mkdir -p \"$build_dir\""
    , ""
    , "# extract commit to build directory"
    , "git archive --worktree-attributes $new_commit | tar x -C \"$build_dir\""
    , ""
    , "pushd \"$build_dir\""
    , "export PATH=${HOME}/.ghcup/bin:$PATH"
    , ""
    , "./build.hs"
    , ""
    , "if [[ refs/heads/master == \"$branch\" ]]; then"
    , "  [[ -d ${HOME}/.local/bin ]] || mkdir -p ${HOME}/.local/bin"
    , "  [[ -f ${HOME}/.local/bin/sensei-exe ]] && rm ${HOME}/.local/bin/sensei-exe"
    , "  cp \"$build_dir/bin/sensei-exe\" ${HOME}/.local/bin/"
    , "  sudo systemctl restart sensei"
    , "fi"
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

  haskellInstalled =
    check
      doesNotHaveHaskell
      ( userScriptProperty
          user
          [ "export BOOTSTRAP_HASKELL_NONINTERACTIVE=1"
          , "curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh"
          ]
      )
      `requires` Apt.installed
        [ "build-essential"
        , "curl"
        , "libffi-dev"
        , "libffi7"
        , "libgmp-dev"
        , "libgmp10"
        , "libncurses-dev"
        , "libncurses6"
        , "libtinfo6"
        ]
      `describe` "GHCUp toolchain installed"

  doesNotHaveHaskell = do
    home <- User.homedir user
    not <$> doesDirectoryExist (home </> ".ghcup" </> "bin")

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

  senseiWebConfig =
    [ "server {"
    , "    listen 80;"
    , "    listen [::]:80;"
    , "    "
    , "    root /var/www/sensei.pankzsoft.net/public_html;"
    , "    index index.html index.htm index.nginx-debian.html;"
    , "    "
    , "    server_name sensei.pankzsoft.net;"
    , "    "
    , "    listen 443 ssl; # managed by Certbot"
    , ""
    , "    # RSA certificate"
    , "    ssl_certificate /etc/letsencrypt/live/sensei.pankzsoft.net/fullchain.pem; # managed by Certbot"
    , "    ssl_certificate_key /etc/letsencrypt/live/sensei.pankzsoft.net/privkey.pem; # managed by Certbot"
    , ""
    , "    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot"
    , ""
    , "    # Redirect non-https traffic to https"
    , "    if ($scheme != \"https\") {"
    , "        return 301 https://$host$request_uri;"
    , "    } # managed by Certbot"
    , ""
    , "    location / {"
    , "        proxy_pass http://127.0.0.1:23456;"
    , "        proxy_set_header X-Real-IP $remote_addr;"
    , "        proxy_set_header X-Forwarded-Proto $scheme;"
    , "    }"
    , "}"
    ]

  cgit =
    [ "server {"
    , "    server_name  git.pankzsoft.net;"
    , ""
    , "    root /var/www/git.pankzsoft.net/public_html;"
    , "    index index.html index.htm index.nginx-debian.html;"
    , "    "
    , "    server_name git.pankzsoft.net;"
    , "    "
    , "    listen 443 ssl; # managed by Certbot"
    , ""
    , "    # RSA certificate"
    , "    ssl_certificate /etc/letsencrypt/live/git.pankzsoft.net/fullchain.pem; # managed by Certbot"
    , "    ssl_certificate_key /etc/letsencrypt/live/git.pankzsoft.net/privkey.pem; # managed by Certbot"
    , ""
    , "    include /etc/letsencrypt/options-ssl-nginx.conf; # managed by Certbot"
    , ""
    , "    # Redirect non-https traffic to https"
    , "    if ($scheme != \"https\") {"
    , "        return 301 https://$host$request_uri;"
    , "    } # managed by Certbot"
    , ""
    , "    try_files $uri @cgit;"
    , ""
    , "    location @cgit {"
    , "        include          fastcgi_params;"
    , "        fastcgi_param    SCRIPT_FILENAME /usr/lib/cgit/cgit.cgi;"
    , "        fastcgi_pass     unix:/run/fcgiwrap.socket;"
    , ""
    , "        fastcgi_param    PATH_INFO    $uri;"
    , "        fastcgi_param    QUERY_STRING $args;"
    , "        fastcgi_param    HTTP_HOST    $server_name;"
    , "        fastcgi_param    REQUEST_METHOD  $request_method;"
    , "        fastcgi_param    CONTENT_TYPE  $content_type;"
    , "        fastcgi_param    CONTENT_LENGTH  $content_length;"
    , "        fastcgi_param    GATEWAY_INTERFACE  CGI/1.1;"
    , "        fastcgi_param    SERVER_SOFTWARE  nginx;"
    , "        fastcgi_param    SCRIPT_NAME  $fastcgi_script_name;"
    , "        fastcgi_param    REQUEST_URI  $request_uri;"
    , "        fastcgi_param    DOCUMENT_URI  $document_uri;"
    , "        fastcgi_param    DOCUMENT_ROOT  $document_root;"
    , "        fastcgi_param    SERVER_PROTOCOL  $server_protocol;"
    , "        fastcgi_param    REMOTE_ADDR  $remote_addr;"
    , "        fastcgi_param    REMOTE_PORT  $remote_port;"
    , "        fastcgi_param    SERVER_ADDR  $server_addr;"
    , "        fastcgi_param    SERVER_PORT  $server_port;"
    , "        fastcgi_param    SERVER_NAME  $server_name;"
    , "    }"
    , ""
    , "    location ~ \\.(css|gif|jpg|png|ico)$ {"
    , "        root /usr/share/cgit/ ;"
    , "        try_files $uri $uri/ =404 ;"
    , "    }"
    , "}"
    ]

  firewall :: Property OS
  firewall =
    propertyList "firewall accepts ssh and web " $
      props
        & flush INPUT
        & firewallPreamble
        & Firewall.rule INPUT Filter ACCEPT (InIFace "wg0")
        & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 22))
        & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 80))
        & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 443))
        & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 5551))
        & dropEverything

  senseiServerInstalled :: Property OS
  senseiServerInstalled =
    withPrivData (PrivFile "sensei-server-jwk") hostContext $ \getPrivDataPwd ->
      property' "sensei service running" $ \w -> do
        getPrivDataPwd $ \(PrivData serverKey) -> do
          dir <- liftIO $ homedir user
          ensureProperty
            w
            ( propertyList "service configured" $
                props
                  & File.hasContent (dir </> ".config" </> "sensei" </> "sensei.jwk") [serverKey]
                  & File.hasContent (dir </> ".sensei.environment") (senseiEnv dir serverKey)
                  & File.hasContent "/etc/systemd/system/sensei.service" (senseiService dir)
                  & Systemd.enabled "sensei"
                  & Systemd.restarted "sensei"
            )

  senseiEnv dir serverKey =
    [ "SENSEI_SERVER_CONFIG_DIR=" <> dir </> ".config" </> "sensei"
    , "SENSEI_SERVER_KEY=" <> serverKey
    , "SENSEI_SERVER_NAME=sensei.pankzsoft.net"
    , "SENSEI_SERVER_PORT=23456"
    , "SENSEI_SERVER_ROOT_USER=arnaud"
    , "SENSEI_SERVER_ROOT_PASSWORD=[\"a0PpgdW5p18oVNdHGlvwqw==\",\"JDJiJDEwJFl5Tm5lYlUzbno2bVRMYkZFanR1b3VtaFNXbVVpcURBRkN0bnJuazEvdUtxVGJYVEhmVENH\"]"
    , "ENVIRONMENT=prod"
    ]

  senseiService dir =
    [ "[Unit]"
    , "Description=Sensei"
    , "After=multi-user.target"
    , ""
    , "[Service]"
    , "EnvironmentFile=" <> dir </> ".sensei.environment"
    , "Type=simple"
    , "ExecStart=" <> dir </> ".local" </> "bin" </> "sensei-exe server"
    , "KillSignal = SIGINT"
    , "RestartKillSignal = SIGINT"
    , "StandardOutput=journal"
    , "StandardError=journal"
    , "SyslogIdentifier=sensei"
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

cgitInstalled :: Property OS
cgitInstalled =
  tightenTargets $
    propertyList "cgit installed" $
      props
        & Apt.installed ["git", "cgit", "fcgiwrap"]
        & User.accountFor (User "git")
        & User.hasLoginShell (User "git") "/usr/bin/git-shell"
        & gitUserHasAuthorizedKeys authorizedKeys
        & gitRepositories ["dotfiles", "lambda-nantes"]
        & bareRepoDefaultBranch (User "git") "lambda-nantes" "main"
        & User.hasGroup (User "www-data") (Group "git")
        & "/etc/cgitrc"
          `File.hasContent` [ "clone-url=https://git.pankzsoft.net/$CGIT_REPO_URL git@git.pankzsoft.net:$CGIT_REPO_URL"
                            , "enable-http-clone=1"
                            , "root-title=Pankzsoft git repositories"
                            , "root-desc="
                            , "enable-index-owner=0"
                            , "snapshots=tar.gz"
                            , "enable-git-config=1"
                            , "scan-path=/home/git"
                            , "virtual-root=/"
                            ]
          `describe` "cgit configured"
        & Systemd.started "fcgiwrap"
 where
  gitRepositories =
    flip describe "Git repositories present"
      . mconcat
      . map (\repo -> Git.bareRepo repo (User "git") Git.SharedAll)

  gitUserHasAuthorizedKeys =
    flip describe "Git authorized users configured"
      . mconcat
      . map (Ssh.authorizedKey (User "git"))

  authorizedKeys =
    [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC8aDeQyneOJA8KJegRWsJyf7qWbyKet5j0GACCDw7KS arnaud@Arnaud-MBP-Perso.local"
    , "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPKjKQVBrq9YUm7nOrcMXXWJnw7lfUk9wp3/MWrfEhgH xavier.vdw@gmail.com"
    , "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDCjxwnPDdr0lQqKiQbDpNaCdvbiNbT1afswe1eIaEk/1wKuAYu4fs/V6QdvHeLhheNOpGC6WOe6eGd07aBp65MPeQm/NhLgsw9PAbQ9zYi+ReoyXYbfwqqEArZIMvlwUqNAuwoG2Q+LtUVChYSn4N+COrWgpzKyfSwh97/0KjtFdm9jfekteaIeFzfl3Fq1sLotBw2fWoLi0tzbKs2J8Q1NxgdmcS9StUw6xIUZ0dDc9b8wC85gvPQS2TrnIh1hfhe5753n6V/48quKrCBBlzHJlwyseaVihChJiDIkqCigerVD4jN2BmEucLMOEiGaXunpVftsGuYRoQ5so7Jd1HH"
    ]

cardano :: Host
cardano =
  host "cardano.hydra.bzh" $
    props
      & osDebian Unstable X86_64
      & alias perasStaging
      & Apt.stdSourcesList
      & Apt.unattendedUpgrades
      & Apt.installed ["etckeeper"]
      & Apt.installed ["ssh", "jq", "tmux", "dstat"]
      & Ssh.installed
      & Tor.installed
      & Tor.hiddenServiceAvailable "ssh" (Port 22)
      & Tor.hiddenServiceAvailable "radicle" (Port 8776)
      & User.hasSomePassword (User "root")
      & File.dirExists "/var/www"
      & File.ownerGroup "/var/www" user userGrp
      & Cron.runPropellor (Cron.Times "30 * * * *")
      & Systemd.persistentJournal
      & firewall
      & Cardano.setup user Mainnet
        `requires` commonUserSetup user
      & Systemd.stopped "hydra-node"
      & Sudo.enabledFor user
      ! httpsWebSite perasStaging [] "me@cardano-scaling.org"
      ! Systemd.nspawned perasContainer
      & Apt.removed ["nginx"]
      & Radicle.radicleSeedInstalled
      & Wireguard.serverInstalled
      & Cardano.tartarusSetup user
 where
  perasContainer =
    Systemd.debContainer "peras" $
      props
        & osDebian Unstable X86_64
        & Apt.removed ["exim4", "exim4-base", "exim4-daemon-light"]
          `onChange` Apt.autoRemove
        & Apt.stdSourcesList `onChange` Apt.upgrade
        & Apt.unattendedUpgrades
        & Apt.cacheCleaned
        & User.hasSomePassword' (User "root") (Context "peras")

  perasStaging = "peras-staging.cardano-scaling.org"

  user = User "curry"
  userGrp = Group "curry"

firewall :: Property OS
firewall =
  propertyList "firewall accepts ssh, web, and Cardano relay " $
    props
      & flush INPUT
      & firewallPreamble
      & Firewall.rule INPUT Filter ACCEPT (InIFace "wg0")
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 22))
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 80))
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 443))
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 3001))
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 5001))
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 5002))
      & Firewall.rule INPUT Filter ACCEPT (Proto TCP :- DPort (Port 8776))
      & Firewall.rule INPUT Filter ACCEPT (Proto UDP :- DPort (Port 51820))
      & dropEverything

-- | A basic rule to drop every input packet
--
--  This should be used as last clause for a bunch of rules, like:
dropEverything :: Property Linux
dropEverything = rule INPUT Filter DROP Everything

-- | Drop all rules for given chain.
--
--  Useful at start of configuration of firewall rules
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
      , "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"
      , "YT90qFF93M3v01BbxP+EIY2/9tiIPbrd"
      , "=0YYh"
      , "-----END PGP PUBLIC KEY BLOCK-----"
      ]

bareRepoDefaultBranch :: User -> FilePath -> String -> Property UnixLike
bareRepoDefaultBranch user repo branch =
  userScriptProperty
    user
    [ "GIT_DIR=" <> repo <> " git symbolic-ref HEAD refs/heads/" ++ branch
    ]
    `changesFileContent` (repo </> "HEAD")
    `describe` ("git repo at " ++ repo ++ " has default branch " ++ branch)
