{-# LANGUAGE NamedFieldPuns #-}

module Radicle where

import Base (OSNoInfo)
import Propellor
import Propellor.Base (doesFileExist, readProcess, (<.>), (</>))
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

seedInstalled :: String -> RevertableProperty OSNoInfo OSNoInfo
seedInstalled userName =
    setupRadicleSeed <!> teardownRadicleSeed
  where
    setupRadicleSeed =
        propertyList "Radicle seed installed" $
            props
                & User.systemAccountFor' user (Just "/home/seed") (Just group)
                & downloadAndInstall (Package "radicle" radicleKey radicleUrl radicleSigUrl radicleSHA256Url)
                & downloadAndInstall (Package "radicle-http" radicleHttpKey radicleHttpUrl radicleHttpSigUrl radicleHttpSHA256Url)

    teardownRadicleSeed =
        tightenTargets $
            propertyList "Radicle seed removed" $
                props
                    & User.nuked user User.YesReallyDeleteHome

    user = User userName
    group = Group userName

    radicleKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL460KIEccS4881p7PPpiiQBsxF+H5tgC6De6crw9rbU"
    radicleUrl = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz"
    radicleSigUrl = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz.sig"
    radicleSHA256Url = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz.sha256"

    radicleHttpKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKU7IHRsae2q1/qd8NaWxfGhPEFGHwK1dcxvSjNdttjb"
    radicleHttpUrl = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz"
    radicleHttpSigUrl = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz.sig"
    radicleHttpSHA256Url = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz.sha256"

    downloadAndInstall Package{name, url, sha256Url} =
        propertyList ("Download and install " <> name) $
            props
                & check
                    (shouldDownload sha256Url archivePath)
                    ( cmdProperty
                        "curl"
                        ["-o", archivePath, "-L", url]
                        `changesFileContent` archivePath
                    )
                    `describe` ( "Archive for "
                                    <> name
                                    <> " downloaded"
                               )
                & File.ownerGroup archivePath user group
      where
        archivePath = "/home" </> userName </> name <.> "tar.xz"

    -- & check
    --     shouldUnpack
    --     ( cmdProperty
    --         "tar"
    --         ["xC", home, "-f", archivePath]
    --         `changesFileContent` ("home" </> "bin" </> "cardano-node")
    --     )
    --     `describe` "Cardano node 9.1.1 archive unpacked"

    shouldDownload sha256Url archivePath = do
        sha256 <- head . words . head . lines <$> readProcess "curl" ["-o", "-", "-L", sha256Url]
        hasFile <- doesFileExist archivePath
        if not hasFile
            then pure True
            else (/= sha256) . head . words . head . lines <$> readProcess "/usr/bin/sha256sum" [archivePath]

data Package = Package
    { name :: String
    , key :: String
    , url :: String
    , sigUrl :: String
    , sha256Url :: String
    }

-- downloadAndInstall :: String -> Package -> Property
