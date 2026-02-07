{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Caddy (
  CaddyConfiguration (..),
  PortNumber,
  caddySiteConfigured,
  caddyServiceConfiguredFor,
) where

import Base (OS)
import Data.Word (Word16)
import Propellor
import Propellor.Base (withPrivData)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd

type PortNumber = Word16

data CaddyConfiguration
  = ReverseProxy HostName PortNumber
  | StaticFiles FilePath
  | WithBasicAuth CaddyConfiguration

toConfigBlock :: Maybe String -> HostName -> CaddyConfiguration -> [String]
toConfigBlock htPasswdContent domain configuration =
  (domain <> " {") : map ("  " <>) directives ++ ["}"]
 where
  directives =
    case configuration of
      (ReverseProxy target port) ->
        [ "  reverse_proxy " <> target <> ":" <> show port
        ]
      (StaticFiles directory) ->
        [ "  root * " <> directory
        , "  file_server"
        ]
      (WithBasicAuth config) ->
        maybe [] authDirective htPasswdContent <> toConfigBlock htPasswdContent domain config
       where
        authDirective passwdContent =
          "basicauth  {"
            : fmap
              (\(userName, passwordHash) -> "  " <> userName <> " " <> passwordHash)
              (foldMap parseLine (lines passwdContent))
              <> ["}"]

        parseLine :: String -> [(UserName, String)]
        parseLine line =
          case break (== ':') line of
            (name, ':' : hash) -> [(name, hash)]
            _ -> []

-- | Configures a Caddy site for the given domain, with the given configuration.
--
-- If a password file is provided, it is used to configure basic
-- authentication for the site. The password file should be in the
-- format of htpasswd, with lines of the form "username:passwordhash".
caddySiteConfigured :: HostName -> CaddyConfiguration -> Maybe FilePath -> Property OS
caddySiteConfigured domain config = \case
  Nothing -> property' ("Caddy site " <> domain <> " configured") $ \w ->
    siteConfiguredProperty w Nothing
  Just passwdFile ->
    withPrivData (PrivFile passwdFile) (Context domain) $ \getHtpasswd ->
      property' ("Caddy site " <> domain <> " configured") $ \w ->
        getHtpasswd $ \(PrivData htpasswdContent) ->
          siteConfiguredProperty w (Just htpasswdContent)
 where
  siteConfiguredProperty w passwdContent =
    ensureProperty w $
      tightenTargets (File.hasContent siteFilePath (toConfigBlock passwdContent domain config))
        <> Systemd.restarted "caddy"

  siteFilePath = "/etc/caddy/sites/" <> domain <> ".caddy"

caddyServiceConfiguredFor :: User -> Property OS
caddyServiceConfiguredFor (User userName) =
  tightenTargets $
    propertyList "Caddy configured" $
      props
        & Apt.installed ["caddy"]
        & File.hasContent "/etc/caddy/Caddyfile" caddyFile
        & File.hasContent "/etc/systemd/system/caddy.service" caddyService
        & File.dirExists "/etc/caddy/sites"
        & Systemd.enabled "caddy"
        & Systemd.restarted "caddy"
 where
  caddyFile = ["import /etc/caddy/sites/*"]

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
