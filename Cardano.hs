{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano where

import Base (OSNoInfo)
import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesDirectoryExist, doesFileExist, readProcess, readProcessEnv, writeReadProcessEnv)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStr)
import Text.Printf (printf)
import Text.Read (readMaybe)

data CardanoNetwork = Mainnet | Preview
    deriving stock (Eq, Show)

setup :: User -> CardanoNetwork -> RevertableProperty OSNoInfo OSNoInfo
setup user@(User userName) network = setupCardanoNode <!> teardownCardanoNode
  where
    -- FIXME: How do I make this part of a property?
    home = "/home" </> userName

    teardownCardanoNode :: Property OSNoInfo
    teardownCardanoNode =
        propertyList "Remove Cardano node" $
            props
                & check
                    (doesFileExist "/etc/systemd/system/cardano-node.service")
                    (Systemd.disabled "cardano-node" `requires` Systemd.stopped "cardano-node")
                & File.notPresent "/etc/systemd/system/cardano-node.service"
                & File.notPresent (home </> "cardano-node.environment")
                & File.notPresent (home </> "cardano-node")
                & File.notPresent (home </> "mithril-client.environment")
                & File.notPresent "/root/mithril-client.deb"
                & Apt.removed ["mithril-client"]
                & File.notPresent (home </> "cardano-configurations")
                & File.notPresent (home </> "cardano-node")

    setupCardanoNode :: Property OSNoInfo
    setupCardanoNode =
        propertyList "Cardano node" $
            props
                & check
                    (shouldDownload sha256 archivePath)
                    ( cmdProperty
                        "curl"
                        ["-o", archivePath, "-L", "https://github.com/IntersectMBO/cardano-node/releases/download/9.1.1/cardano-node-9.1.1-linux.tar.gz"]
                        `changesFileContent` archivePath
                    )
                    `describe` "Cardano node 9.1.1 archive downloaded"
                & File.ownerGroup archivePath user userGrp
                & check
                    shouldUnpack
                    ( cmdProperty
                        "tar"
                        ["xC", home, "-f", archivePath]
                        `changesFileContent` (home </> "bin" </> "cardano-node")
                    )
                    `describe` "Cardano node 9.1.1 archive unpacked"
                & environmentConfigured
                & File.hasContent "/etc/systemd/system/cardano-node.service" serviceNode
                & Apt.removed ["mithril-client"]
                & mithrilSnapshotDownloaded user userGrp network
                & Systemd.enabled "cardano-node"
                & Systemd.restarted "cardano-node"
                & File.notPresent (home </> "cardano-configuration")

    environmentConfigured =
        File.hasContent (home </> "cardano-node.environment") envFile

    sha256 = "48ba01b2f4659922cdccc6ff3a221610ec3b7881254d7bcb16fbb5fc88b1b183"

    shouldUnpack = do
        dir <- User.homedir user
        hasFile <- doesFileExist (dir </> "bin" </> "cardano-node")
        if hasFile
            then
                not
                    . ("9.1.1" `elem`)
                    . words
                    . head
                    . lines
                    <$> readProcessEnv (dir </> "bin" </> "cardano-node") ["--version"] (Just [("LD_LIBRARY_PATH", dir)])
            else pure True

    archivePath = home </> "cardano-node-9.1.1.tgz"

    userGrp = Group "curry"

    envFile =
        [ "CONFIG=\"" <> home </> "share" </> networkName network </> "config.json\""
        , "TOPOLOGY=\"" <> home </> "share" </> networkName network </> "topology.json\""
        , "DBPATH=\"./db/\""
        , "SOCKETPATH=\"./node.socket\""
        , "HOSTADDR=\"0.0.0.0\""
        , "PORT=\"3001\""
        , "LD_LIBRARY_PATH=\"" <> home <> "\""
        ]

    serviceNode =
        [ "[Unit]"
        , "Description=Cardano node"
        , "After=multi-user.target"
        , ""
        , "[Service]"
        , "Type=simple"
        , "EnvironmentFile=" <> home <> "/cardano-node.environment"
        , "ExecStart=" <> home </> "bin" </> "cardano-node run --config $CONFIG --topology $TOPOLOGY --database-path $DBPATH --socket-path $SOCKETPATH --host-addr $HOSTADDR --port $PORT"
        , "KillSignal = SIGINT"
        , "RestartKillSignal = SIGINT"
        , "StandardOutput=journal"
        , "StandardError=journal"
        , "SyslogIdentifier=cardano-node"
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

networkName :: CardanoNetwork -> String
networkName = \case
    Mainnet -> "mainnet"
    Preview -> "preview"

mithrilSnapshotDownloaded ::
    User ->
    Group ->
    CardanoNetwork ->
    Property
        ( MetaTypes
            '[ 'Targeting 'OSDebian
             , 'Targeting 'OSBuntish
             , 'Targeting 'OSArchLinux
             ]
        )
mithrilSnapshotDownloaded user@(User userName) userGrp network =
    propertyList "Mithril snapshot downloaded" $
        props
            & check
                (shouldDownload archiveSha256 mithrilPath)
                ( cmdProperty
                    "curl"
                    ["-o", mithrilPath, "-L", "https://github.com/input-output-hk/mithril/releases/download/2430.0/mithril-client-cli_0.9.9+52a7beb-1_amd64.deb"]
                    `changesFileContent` mithrilPath
                )
                `describe` ("Mithril client " <> mithrilClientVersion <> " package downloaded")
            & check
                shouldUnpack
                ( cmdProperty "dpkg" ["--install", mithrilPath]
                    `assume` MadeChange
                    `describe` ("Mithril client " <> mithrilClientVersion <> " package installed")
                )
            & File.hasContent
                (home </> "mithril-client.environment")
                [ "export AGGREGATOR_ENDPOINT=\"" <> aggregatorEndpoint <> "\""
                , "export GENESIS_VERIFICATION_KEY=\"" <> genesisVerificationKey <> "\""
                ]
            & File.ownerGroup (home </> "mithril-client.environment") user userGrp
            & check
                shouldDownloadSnapshot
                ( userScriptProperty
                    user
                    [ ". ./mithril-client.environment"
                    , "sudo rm -fr db"
                    , "mithril-client cardano-db download " <> mithrilSnapshot
                    ]
                    `assume` MadeChange
                    `describe` ("Install Mithril snapshot " <> mithrilSnapshot)
                    `requires` Systemd.stopped "cardano-node"
                )
  where
    home = "/home" </> userName

    aggregatorEndpoint = case network of
        Preview -> "https://aggregator.pre-release-preview.api.mithril.network/aggregator"
        Mainnet -> "https://aggregator.release-mainnet.api.mithril.network/aggregator"

    genesisVerificationKey = case network of
        Preview -> "5b3132372c37332c3132342c3136312c362c3133372c3133312c3231332c3230372c3131372c3139382c38352c3137362c3139392c3136322c3234312c36382c3132332c3131392c3134352c31332c3233322c3234332c34392c3232392c322c3234392c3230352c3230352c33392c3233352c34345d"
        Mainnet -> "5b3139312c36362c3134302c3138352c3133382c31312c3233372c3230372c3235302c3134342c32372c322c3138382c33302c31322c38312c3135352c3230342c31302c3137392c37352c32332c3133382c3139362c3231372c352c31342c32302c35372c37392c33392c3137365d"

    mithrilSnapshot = "latest"

    archiveSha256 = "0000"

    mithrilPath = "/root/mithril-client.deb"

    mithrilClientVersion = "0.9.9+52a7beb"

    shouldUnpack = do
        let exe = "/usr/bin/mithril-client"
        hasExe <- doesFileExist exe
        if hasExe
            then
                not
                    . (mithrilClientVersion `elem`)
                    . words
                    . head
                    . lines
                    <$> readProcess "/usr/bin/mithril-client" ["--version"]
            else pure True

    shouldDownloadSnapshot = do
        dir <- User.homedir user
        let mithrilEnv =
                [ ("AGGREGATOR_ENDPOINT", aggregatorEndpoint)
                , ("GENESIS_VERIFICATION_KEY", genesisVerificationKey)
                ]

        snapshotJson <- readProcessEnv "/usr/bin/mithril-client" ["cardano-db", "snapshot", "show", mithrilSnapshot, "--json"] (Just mithrilEnv)
        lastImmutableFileNumber <-
            readMaybe . head . lines
                <$> writeReadProcessEnv "jq" ["-c", ".beacon.immutable_file_number"] Nothing (Just $ \hdl -> hPutStr hdl snapshotJson) Nothing
        case lastImmutableFileNumber of
            Nothing -> pure False
            Just (num :: Int) -> do
                let chunkNumber = printf "%05d" num
                    chunkFile = dir </> "db" </> "immutable" </> chunkNumber <.> "chunk"
                foundChunk <- doesFileExist chunkFile
                if foundChunk
                    then putStrLn ("Found chunk: " <> chunkFile) >> pure False
                    else putStrLn ("Cannot find chunk: " <> chunkFile) >> pure True

shouldDownload :: String -> FilePath -> IO Bool
shouldDownload sha256 archivePath = do
    hasFile <- doesFileExist archivePath
    if not hasFile
        then pure True
        else (/= sha256) . head . words . head . lines <$> readProcess "/usr/bin/sha256sum" [archivePath]
