{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module Radicle where

import Base (OS)
import qualified Data.List as List
import Propellor
import Propellor.Base (
    asks,
    doesDirectoryExist,
    doesFileExist,
    readProcess,
    removeDirectoryRecursive,
    withPrivData,
    (<.>),
    (</>),
 )
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User

radiclePackage :: Package
radiclePackage =
    Package "radicle" radicleKey radicleUrl radicleSigUrl radicleSHA256Url radicleVersion
  where
    radicleVersion = "1.0.0-rc.17"
    radicleKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIL460KIEccS4881p7PPpiiQBsxF+H5tgC6De6crw9rbU"
    radicleUrl = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz"
    radicleSigUrl = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz.sig"
    radicleSHA256Url = "https://files.radicle.xyz/releases/latest/radicle-1.0.0-rc.17-x86_64-unknown-linux-musl.tar.xz.sha256"

radicleHttpPackage :: Package
radicleHttpPackage =
    Package "radicle-http" radicleHttpKey radicleHttpUrl radicleHttpSigUrl radicleHttpSHA256Url radicleHttpVersion
  where
    radicleHttpVersion = "0.17.0"
    radicleHttpKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKU7IHRsae2q1/qd8NaWxfGhPEFGHwK1dcxvSjNdttjb"
    radicleHttpUrl = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz"
    radicleHttpSigUrl = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz.sig"
    radicleHttpSHA256Url = "https://files.radicle.xyz/releases/radicle-httpd/latest/radicle-httpd-0.17.0-x86_64-unknown-linux-musl.tar.xz.sha256"

radicleSeedInstalled :: RevertableProperty OS OS
radicleSeedInstalled =
    setupRadicleSeed <!> teardownRadicleSeed
  where
    userName = "seed"

    home = "/home" </> userName
    group = Group userName
    user = User userName

    usrLocal = "/usr/local"
    radicleDir = home </> ".radicle"
    archivePath name = "/tmp" </> name <.> "tar.xz"

    setupRadicleSeed =
        tightenTargets $
            propertyList "Radicle seed installed" $
                props
                    & User.accountFor user
                    & User.systemGroup group
                    & User.hasGroup user group
                    & File.dirExists radicleDir
                    & File.ownerGroup radicleDir user group
                    & downloadAndInstall
                        user
                        group
                        usrLocal
                        (archivePath "radicle")
                        radiclePackage
                    & downloadAndInstall
                        user
                        group
                        usrLocal
                        (archivePath "radicle-http")
                        radicleHttpPackage
                    & configureRadicle user

    teardownRadicleSeed =
        tightenTargets $
            propertyList "Radicle seed removed" $
                props
                    & User.nuked user User.YesReallyDeleteHome

radicleInstalledFor :: User -> RevertableProperty OS OS
radicleInstalledFor user@(User userName) =
    setupRadicle <!> teardownRadicle
  where
    setupRadicle =
        propertyList "Radicle installed" $
            props
                & File.dirExists radicleDir
                & File.ownerGroup radicleDir user group
                & downloadAndInstall
                    user
                    group
                    radicleDir
                    (archivePath "radicle")
                    radiclePackage
                & configureRadicle user
                & nodeRunning user (radicleDir </> "bin" </> "rad")

    teardownRadicle =
        tightenTargets $
            propertyList "Radicle removed" $
                props
                    & nodeStopped
                    & dirNotPresent radicleDir

    nodeStopped :: Property OS
    nodeStopped =
        withPrivData (PrivFile "radicle-pwd") hostContext $ \getPrivDataPwd ->
            property' "radicle node stopped" $ \w -> do
                getPrivDataPwd $ \(PrivData privDataPwd) ->
                    ensureProperty
                        w
                        ( userScriptPropertyPty
                            user
                            [ "export RAD_PASSPHRASE=" <> privDataPwd
                            , radicleDir </> "bin" </> "rad node stop"
                            ]
                            `assume` NoChange
                        )

    -- FIXME: should not be hardcoded
    home = "/home" </> userName
    group = Group userName

    radicleDir = home </> ".radicle"
    archivePath name = home </> name <.> "tar.xz"

downloadAndInstall ::
    User ->
    Group ->
    FilePath ->
    FilePath ->
    Package ->
    Property OS
downloadAndInstall user group installDir archive Package{name, url, sha256Url, version} =
    tightenTargets $
        check (shouldUnpack (installDir </> name) version) $
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

shouldDownload :: String -> FilePath -> IO Bool
shouldDownload sha256Url archive = do
    sha256 <- head . words . head . lines <$> readProcess "curl" ["-o", "-", "-L", sha256Url]
    hasFile <- doesFileExist archive
    if not hasFile
        then pure True
        else (/= sha256) . head . words . head . lines <$> readProcess "/usr/bin/sha256sum" [archive]

shouldUnpack :: FilePath -> String -> IO Bool
shouldUnpack exe radicleVersion = do
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

nodeRunning :: User -> FilePath -> Property OS
nodeRunning user radExe =
    withPrivData (PrivFile "radicle-pwd") hostContext $ \getPrivDataPwd ->
        property' "radicle node running" $ \w -> do
            getPrivDataPwd $ \(PrivData privDataPwd) ->
                ensureProperty
                    w
                    ( userScriptPropertyPty
                        user
                        [ "export RAD_PASSPHRASE=" <> privDataPwd
                        , radExe <> " node start"
                        ]
                        `assume` NoChange
                    )

configureRadicle :: User -> Property OS
configureRadicle user@(User userName) =
    withPrivData (PrivFile "radicle-seed") hostContext $ \getPrivDataSeed ->
        withPrivData (PrivFile "radicle-pwd") hostContext $ \getPrivDataPwd ->
            property' "radicle configured" $ \w -> do
                host <- asks hostName
                getPrivDataSeed $ \(PrivData privDataSeed) ->
                    getPrivDataPwd $ \(PrivData privDataPwd) ->
                        ensureProperty w (radAuth user (userName <.> host) privDataSeed privDataPwd)

radAuth :: User -> String -> String -> String -> Property UnixLike
radAuth user nodeName privDataSeed privDataPwd =
    check keysExist $
        userScriptPropertyPty
            user
            [ "export RAD_KEYGEN_SEED=" <> privDataSeed
            , "export RAD_PASSPHRASE=" <> privDataPwd
            , "rad auth --alias " <> nodeName
            ]
  where
    keysExist = do
        radicleDir <- User.homedir user
        doesFileExist (radicleDir </> "keys/radicle")

data Package = Package
    { name :: String
    , key :: String
    , url :: String
    , sigUrl :: String
    , sha256Url :: String
    , version :: String
    }

-- | Removes a directory, and all its contents.
dirNotPresent :: FilePath -> Property UnixLike
dirNotPresent dir =
    check (doesDirectoryExist dir) $
        property (dir ++ " not present") $
            makeChange $
                removeDirectoryRecursive dir

{- | A property that can satisfied by running a script
as user (cd'd to their home directory), allocating a pseudo-terminal.
This is important if the commands being run need to interact expect
the input to be a terminal.

NOTE: The `Script`'s lines should not end with a newline.
-}
userScriptPropertyPty :: User -> Script -> UncheckedProperty UnixLike
userScriptPropertyPty (User user) script =
    cmdProperty
        "su"
        ["-P", "--login", "--shell", "/bin/sh", "-c", shellcmd, user]
  where
    shellcmd = List.intercalate " ; " ("set -e" : "cd" : script)
