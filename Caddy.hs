{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Caddy (
  CaddyConfiguration (..),
  CGIConfiguration (..),
  Matcher (..),
  Transport (..),
  ReverseProxyOption (..),
  PortNumber,
  caddySiteConfigured,
  caddyServiceConfiguredFor,
  toConfigBlock,
) where

import Base (OS)
import Data.String (IsString)
import Data.Word (Word16)
import Propellor
import Propellor.Base (withPrivData)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd

type PortNumber = Word16

data Matcher
  = PathMatcher String
  | HeaderMatcher String String
  | Name String
  | PathRegexMatcher String String
  deriving (Eq)

instance Show Matcher where
  show (PathMatcher path) = path
  show (HeaderMatcher header value) = "header " <> header <> " " <> value
  show (PathRegexMatcher regex path) = "path_regexp " <> regex <> " " <> path
  show (Name name) = "@" <> name

instance IsString Matcher where
  fromString = PathMatcher

configureMatcher :: Matcher -> String
configureMatcher (PathMatcher path) = "path " <> path
configureMatcher (HeaderMatcher header value) = "header " <> header <> " " <> value
configureMatcher (PathRegexMatcher regex path) = "path_regexp " <> regex <> " " <> path
configureMatcher (Name name) = "@" <> name

data CaddyConfiguration
  = ReverseProxy HostName PortNumber [ReverseProxyOption]
  | StaticFiles FilePath
  | WithBasicAuth CaddyConfiguration
  | Route [CaddyConfiguration]
  | Handle Matcher [CaddyConfiguration]
  | HandlePath String [CaddyConfiguration]
  | NamedMatcher String [Matcher]
  | CGI CGIConfiguration
  | Directives [CaddyConfiguration]
  deriving (Show, Eq)

data ReverseProxyOption
  = Transport Transport
  | HeaderUp String String
  deriving (Show, Eq)

data Transport = FastCGI {envs :: [(String, String)]}
  deriving (Show, Eq)

data CGIConfiguration = CGIConfiguration
  { cgiExecutable :: FilePath
  , environment :: [(String, String)]
  }
  deriving (Show, Eq)

toConfigBlock :: Maybe String -> HostName -> CaddyConfiguration -> [String]
toConfigBlock htPasswdContent domain configuration =
  (domain <> " {") : map ("  " <>) (directives configuration) ++ ["}"]
 where
  directives config =
    case config of
      (Directives configs) -> concatMap directives configs
      (NamedMatcher name matchers) ->
        let matcherLines = map configureMatcher matchers
         in "@" <> name <> " {" : matcherLines ++ ["}"]
      (CGI cgiConfig) ->
        let env = "env " <> foldMap (\(key, value) -> key <> " " <> value <> " ") (environment cgiConfig)
         in "  cgi " <> cgiExecutable cgiConfig <> " {" : env : ["}"]
      (Handle matcher configs) ->
        "handle " <> show matcher <> " { " : concatMap directives configs <> ["}"]
      (HandlePath matcher configs) ->
        "handle_path " <> show matcher <> " { " : concatMap directives configs <> ["}"]
      (Route configs) ->
        "route { " : concatMap directives configs <> ["}"]
      (ReverseProxy target port proxyOptions) ->
        ("  reverse_proxy " <> target <> ":" <> show port <> " {")
          : concatMap reverseProxyOptionDirectives proxyOptions <> ["}"]
      (StaticFiles directory) ->
        [ "  root * " <> directory
        , "  file_server"
        ]
      (WithBasicAuth config') ->
        maybe [] authDirective htPasswdContent <> directives config'
       where
        authDirective passwdContent =
          "basic_auth  {"
            : fmap
              (\(userName, passwordHash) -> "  " <> userName <> " " <> passwordHash)
              (foldMap parseLine (lines passwdContent))
              <> ["}"]

        parseLine :: String -> [(UserName, String)]
        parseLine line =
          case break (== ':') line of
            (name, ':' : hash) -> [(name, hash)]
            _ -> []

reverseProxyOptionDirectives :: ReverseProxyOption -> [String]
reverseProxyOptionDirectives = \case
  (Transport (FastCGI envs)) ->
    let envLines = map (\(key, value) -> "env " <> key <> " " <> value) envs
     in ["transport fastcgi {"] ++ envLines ++ ["}"]
  (HeaderUp name value) ->
    ["header_up " <> name <> " " <> value]

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
      property' ("Caddy site " <> domain <> " configured with .htpasswd " <> passwdFile) $ \w ->
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
    , "StartLimitBurst=20"
    , ""
    , "[Install]"
    , "WantedBy=multi-user.target"
    ]
