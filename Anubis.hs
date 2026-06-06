module Anubis (
  anubisConfigured,
  anubisInstalled,
  anubisPort,
  backendPort,
) where

import Base (OS)
import Data.Word (Word16)
import Propellor
import Propellor.Base (doesFileExist, readProcess)
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd

anubisVersion :: String
anubisVersion = "1.25.0"

anubisPort :: Word16
anubisPort = 8923

backendPort :: Word16
backendPort = 8924

anubisInstalled :: Property OS
anubisInstalled =
  tightenTargets $
    check shouldInstall $
      propertyList "Anubis installed" $
        props
          & ( cmdProperty
                "curl"
                ["-o", archivePath, "-L", anubisUrl]
                `changesFileContent` archivePath
            )
            `describe` ("Anubis " <> anubisVersion <> " downloaded")
          & ( cmdProperty
                "tar"
                ["-C", "/usr/local/bin", "--strip-components", "1", "-xzf", archivePath, tarBinaryPath]
                `changesFileContent` binaryPath
            )
            `describe` ("Anubis " <> anubisVersion <> " installed")
 where
  anubisUrl =
    "https://github.com/TecharoHQ/anubis/releases/download/v"
      <> anubisVersion
      <> "/anubis-"
      <> anubisVersion
      <> "-linux-amd64.tar.gz"

  archivePath = "/tmp/anubis-" <> anubisVersion <> ".tar.gz"
  binaryPath = "/usr/local/bin/anubis"
  tarBinaryPath = "anubis-" <> anubisVersion <> "-linux-amd64/bin/anubis"

  shouldInstall = do
    hasFile <- doesFileExist binaryPath
    if hasFile
      then do
        output <- readProcess binaryPath ["version"]
        pure $ anubisVersion `notElem` words (head (lines output))
      else pure True

anubisConfigured :: String -> FilePath -> Property OS
anubisConfigured siteName staticDir =
  tightenTargets $
    propertyList ("Anubis configured for " <> siteName) $
      props
        & File.hasContent servicePath serviceFile
        & File.hasContent backendCaddyPath (backendCaddyFile staticDir)
        & Systemd.enabled serviceName
        & Systemd.restarted serviceName
          `requires` anubisInstalled
 where
  serviceName = "anubis-" <> siteName

  servicePath = "/etc/systemd/system/" <> serviceName <> ".service"

  serviceFile =
    [ "[Unit]"
    , "Description=Anubis bot protection for " <> siteName
    , "After=network.target"
    , ""
    , "[Service]"
    , "Type=simple"
    , "ExecStart=/usr/local/bin/anubis"
    , "Environment=BIND=:" <> show anubisPort
    , "Environment=TARGET=http://127.0.0.1:" <> show backendPort
    , "Environment=DIFFICULTY=4"
    , "Restart=always"
    , "RestartSec=3"
    , ""
    , "[Install]"
    , "WantedBy=multi-user.target"
    ]

  backendCaddyPath = "/etc/caddy/sites/" <> siteName <> "-backend.caddy"

  backendCaddyFile dir =
    [ ":" <> show backendPort <> " {"
    , "  bind 127.0.0.1"
    , "  root * " <> dir
    , "  file_server"
    , "}"
    ]
