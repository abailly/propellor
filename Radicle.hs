{-# LANGUAGE NamedFieldPuns #-}

module Radicle where

import Base (OSNoInfo)
import Propellor
import Propellor.Base (doesDirectoryExist, doesFileExist, readProcess, removeDirectoryRecursive, (<.>), (</>))
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

radicleInstalledFor :: User -> RevertableProperty OSNoInfo OSNoInfo
radicleInstalledFor user@(User userName) =
    setupRadicle <!> teardownRadicle
  where
    setupRadicle =
        tightenTargets $
            propertyList "Radicle installed" $
                props
                    & User.nuked (User "seed") User.YesReallyDeleteHome
                    & File.dirExists radicleDir
                    & File.dirExists installDir
                    & File.ownerGroup radicleDir user group
                    & downloadAndInstall
                        installDir
                        (archivePath "radicle")
                        (Package "radicle" radicleKey radicleUrl radicleSigUrl radicleSHA256Url radicleVersion)
                    & downloadAndInstall
                        installDir
                        (archivePath "radicle-http")
                        (Package "radicle-http" radicleHttpKey radicleHttpUrl radicleHttpSigUrl radicleHttpSHA256Url radicleHttpVersion)

    teardownRadicle =
        tightenTargets $
            propertyList "Radicle removed" $
                props

    -- FIXME: should not be hardcoded
    home = "/home" </> userName
    group = Group userName

    radicleDir = home </> ".radicle"
    installDir = radicleDir </> "bin"
    archivePath name = home </> name <.> "tar.xz"

    radicleVersion = "1.0.0-rc.17"
    radicleKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL460KIEccS4881p7PPpiiQBsxF+H5tgC6De6crw9rbU"
    radicleUrl = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz"
    radicleSigUrl = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz.sig"
    radicleSHA256Url = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz.sha256"

    radicleHttpVersion = "0.17.0"
    radicleHttpKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKU7IHRsae2q1/qd8NaWxfGhPEFGHwK1dcxvSjNdttjb"
    radicleHttpUrl = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz"
    radicleHttpSigUrl = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz.sig"
    radicleHttpSHA256Url = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz.sha256"

    downloadAndInstall installDir archive Package{name, url, sha256Url, version} =
        check (shouldUnpack $ installDir </> name) $
            propertyList ("Download and install " <> name) $
                props
                    & check
                        (shouldDownload sha256Url archive)
                        ( cmdProperty
                            "curl"
                            ["-o", archive, "-L", url]
                            `changesFileContent` archive
                        )
                        `describe` ( "Archive for "
                                        <> name
                                        <> " downloaded"
                                   )
                    & File.ownerGroup archive user group
                    & ( cmdProperty
                            "tar"
                            ["--strip-components", "1", "-C", installDir, "-xf", archive]
                            `changesFileContent` (installDir </> name)
                      )
                        `describe` ("Radicle " <> version <> " unpacked")

    shouldDownload sha256Url archive = do
        sha256 <- head . words . head . lines <$> readProcess "curl" ["-o", "-", "-L", sha256Url]
        hasFile <- doesFileExist archive
        if not hasFile
            then pure True
            else (/= sha256) . head . words . head . lines <$> readProcess "/usr/bin/sha256sum" [archive]

    shouldUnpack exe = do
        hasFile <- doesFileExist exe
        if hasFile
            then
                not
                    . (radicleVersion `elem`)
                    . words
                    . head
                    . lines
                    <$> readProcess exe ["--version"]
            else pure True

data Package = Package
    { name :: String
    , key :: String
    , url :: String
    , sigUrl :: String
    , sha256Url :: String
    , version :: String
    }

-- downloadAndInstall :: String -> Package -> Property

-- | Removes a directory, and all its contents.
dirNotPresent :: FilePath -> Property UnixLike
dirNotPresent dir =
    check (doesDirectoryExist dir) $
        property (dir ++ " not present") $
            makeChange $
                removeDirectoryRecursive dir
