module Caddy (
  CaddyConfiguration (..),
  PortNumber,
  caddyServiceConfiguredFor,
) where

import Base (OS)
import Data.Word (Word16)
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd

type PortNumber = Word16

data CaddyConfiguration
  = ReverseProxy HostName HostName PortNumber
  | StaticFiles HostName FilePath

caddyServiceConfiguredFor :: User -> [CaddyConfiguration] -> Property OS
caddyServiceConfiguredFor (User userName) configs =
  tightenTargets $
    propertyList "Caddy configured" $
      props
        & Apt.installed ["caddy"]
        & File.hasContent "/etc/caddy/Caddyfile" caddyFile
        & File.hasContent "/etc/systemd/system/caddy.service" caddyService
        & Systemd.enabled "caddy"
        & Systemd.restarted "caddy"
 where
  caddyFile = concatMap configBlock configs

  configBlock (ReverseProxy domain target port) =
    [ domain <> " {"
    , "  reverse_proxy " <> target <> ":" <> show port
    , "}"
    ]
  configBlock (StaticFiles domain directory) =
    [ domain <> " {"
    , "  root * " <> directory
    , "  file_server"
    , "}"
    ]

  caddyService =
    [ "[Unit]"
    , "Description=Caddy"
    , "Documentation=https://caddyserver.com/docs/"
    , "After=network.target network-online.target"
    , "Requires=network-online.target"
    , ""
    , "[Service]"
    , "Type=notify"
    , "User=" <> userName
    , "Group=" <> userName
    , "ExecStart=/usr/bin/caddy run --environ --config /etc/caddy/Caddyfile"
    , "ExecReload=/usr/bin/caddy reload --config /etc/caddy/Caddyfile --force"
    , "TimeoutStopSec=5s"
    , "LimitNOFILE=1048576"
    , "PrivateTmp=true"
    , "ProtectSystem=full"
    , "AmbientCapabilities=CAP_NET_ADMIN CAP_NET_BIND_SERVICE"
    , ""
    , "[Install]"
    , "WantedBy=multi-user.target"
    ]
