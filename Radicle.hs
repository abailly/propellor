{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Radicle where

import Base (OS)
import qualified Data.List as List
import Propellor
import Propellor.Base (
    asks,
    doesDirectoryExist,
    doesFileExist,
    liftIO,
    readProcess,
    removeDirectoryRecursive,
    withPrivData,
    (<.>),
    (</>),
 )
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd
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
                    & serviceConfigured user
                    & Systemd.enabled "radicle-node"
                    & Systemd.restarted "radicle-node"
                    & seeding user "/usr/local" seeds

    teardownRadicleSeed =
        tightenTargets $
            propertyList "Radicle seed removed" $
                props
                    & check
                        (doesFileExist "/etc/systemd/system/radicle-node.service")
                        (Systemd.disabled "radicle-node" `requires` Systemd.stopped "radicle-node")
                    & File.notPresent "/etc/systemd/system/radicle-node.service"
                    & User.nuked user User.YesReallyDeleteHome

    seeds =
        [ SeedFollowed "rad:z3DHQu16u3Do8Da4WMytx36qdanz5" "z6MkgrwQNecpatYWTPnzvZfWt6jpxZq1zK7zuz8QmndpMrGJ"
        ]

data Seed
    = SeedAll
        { repo :: String
        , nid :: String
        }
    | SeedFollowed
        { repo :: String
        , nid :: String
        }

seeding :: User -> FilePath -> [Seed] -> Property OS
seeding user radicleDir seeds =
    withPrivData (PrivFile "radicle-pwd") hostContext $ \getPrivDataPwd ->
        property' "radicle node service file" $ \w ->
            getPrivDataPwd $ \(PrivData privDataPwd) ->
                ensureProperty w $
                    mconcat (map (seeded privDataPwd) seeds)
  where
    seeded pwd seed =
        userScriptPropertyPty
            user
            [ "export RAD_PASSPHRASE=" <> pwd
            , radicleDir </> "bin" </> "rad seed " <> repo seed <> " --from " <> nid seed <> " --scope " <> scope seed
            ]
            `assume` NoChange

    scope = \case
        SeedAll{} -> "all"
        SeedFollowed{} -> "followed"

serviceConfigured :: User -> Property OS
serviceConfigured user@(User userName) =
    tightenTargets $
        propertyList "Radicle node configured" $
            props
                & configFileForNode
                & File.ownerGroup configFilePath user group
                & serviceFileForNode
  where
    group = Group userName

    configFilePath = "/home" </> userName </> ".radicle" </> "config.json"

    configFileForNode :: Property OS
    configFileForNode =
        property' "radicle node config file" $ \w -> do
            host <- asks hostName
            ensureProperty w $
                ( File.hasContent configFilePath (configFile host)
                    <> File.ownerGroup configFilePath user group
                )

    serviceFileForNode :: Property OS
    serviceFileForNode =
        withPrivData (PrivFile "radicle-pwd") hostContext $ \getPrivDataPwd ->
            property' "radicle node service file" $ \w ->
                getPrivDataPwd $ \(PrivData privDataPwd) ->
                    ensureProperty w $
                        File.hasContent "/etc/systemd/system/radicle-node.service" (nodeService privDataPwd)

    -- TODO: should not be hardcoded but deduced from Tor service output?
    onionAddress = "tk7hobv7lpe6axiq6wlkarevp3dsnzvo7s6wuefloe7shbsvnkpm75yd.onion:8776"

    configFile host =
        [ "{"
        , "  \"node\": {"
        , "    \"alias\": \"" <> userName <.> host <> "\","
        , "    \"externalAddresses\": [\"" <> host <> ":8776\", \"" <> onionAddress <> "\"],"
        , "    \"onion\": {"
        , "        \"mode\": \"proxy\","
        , "        \"address\": \"127.0.0.1:9050\""
        , "    },"
        , "    \"seedingPolicy\": {"
        , "      \"default\": \"block\""
        , "    }"
        , "  }"
        , "}"
        ]

    nodeService radiclePwd =
        [ "[Unit]"
        , "Description=Radicle Node"
        , "After=network.target network-online.target"
        , "Requires=network-online.target"
        , ""
        , "[Service]"
        , "User=seed"
        , "Group=seed"
        , "ExecStart=/usr/local/bin/radicle-node --listen 0.0.0.0:8776 --force"
        , "Environment=RAD_HOME=/home/" <> userName </> ".radicle RUST_BACKTRACE=1 RUST_LOG=info RAD_PASSPHRASE=" <> radiclePwd
        , "KillMode=process"
        , "Restart=always"
        , "RestartSec=3"
        , ""
        , "[Install]"
        , "WantedBy=multi-user.target"
        ]

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
                    getPrivDataPwd $ \(PrivData privDataPwd) -> do
                        dir <- liftIO $ User.homedir user
                        ensureProperty
                            w
                            ( radAuth user (userName <.> host) privDataSeed privDataPwd
                                <> File.hasContent (dir </> ".radicle" </> "config.json") (radicleConfig host)
                            )
  where
    radicleConfig host =
        [ "{"
        , "  \"publicExplorer\": \"https://app.radicle.xyz/nodes/$host/$rid$path\","
        , "  \"preferredSeeds\": ["
        , "    \"z6MkrLMMsiPWUcNPHcRajuMi9mDfYckSoJyPwwnknocNYPm7@seed.radicle.garden:8776\","
        , "    \"z6Mkmqogy2qEM2ummccUthFEaaHvyYmYBYh3dbe9W4ebScxo@ash.radicle.garden:8776\","
        , "    \"z6MkfiRENtzUJiU1kxLhxWMWFCiGGxGi6jEbj33Pq9zBVQkK@cardano.hydra.bzh:8776\""
        , "  ],"
        , "  \"web\": {"
        , "    \"pinned\": {"
        , "      \"repositories\": []"
        , "    }"
        , "  },"
        , "  \"cli\": {"
        , "    \"hints\": true"
        , "  },"
        , "  \"node\": {"
        , "    \"alias\": \"" <> userName <.> host <> "\","
        , "    \"listen\": [\"0.0.0.0:8776\"],"
        , "    \"onion\": {"
        , "        \"mode\": \"proxy\","
        , "        \"address\": \"127.0.0.1:9050\""
        , "    },"
        , "    \"peers\": {"
        , "      \"type\": \"dynamic\""
        , "    },"
        , "    \"connect\": [],"
        , "    \"externalAddresses\": [],"
        , "    \"network\": \"main\","
        , "    \"log\": \"INFO\","
        , "    \"relay\": \"auto\","
        , "    \"limits\": {"
        , "      \"routingMaxSize\": 1000,"
        , "      \"routingMaxAge\": 604800,"
        , "      \"gossipMaxAge\": 1209600,"
        , "      \"fetchConcurrency\": 1,"
        , "      \"maxOpenFiles\": 4096,"
        , "      \"rate\": {"
        , "        \"inbound\": {"
        , "          \"fillRate\": 5.0,"
        , "          \"capacity\": 1024"
        , "        },"
        , "        \"outbound\": {"
        , "          \"fillRate\": 10.0,"
        , "          \"capacity\": 2048"
        , "        }"
        , "      },"
        , "      \"connection\": {"
        , "        \"inbound\": 128,"
        , "        \"outbound\": 16"
        , "      }"
        , "    },"
        , "    \"workers\": 8,"
        , "    \"seedingPolicy\": {"
        , "      \"default\": \"block\""
        , "    }"
        , "  }"
        , "}"
        ]

radAuth :: User -> String -> String -> String -> Property UnixLike
radAuth user nodeName privDataSeed privDataPwd =
    check (not <$> keysExist) $
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
