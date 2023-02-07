-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/
{-# LANGUAGE DataKinds #-}

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.User as User
import Propellor.Types.MetaTypes (MetaType (..), MetaTypes)
import Propellor.Utilities (doesFileExist, readProcess)

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
    [ cardano
    ]

-- An example host.
cardano :: Host
cardano =
    host "cardano.hydra.bzh" $
        props
            & osDebian Unstable X86_64
            & Apt.stdSourcesList
            & Apt.unattendedUpgrades
            & Apt.installed ["etckeeper"]
            & Apt.installed ["ssh"]
            & User.hasSomePassword (User "root")
            & File.dirExists "/var/www"
            & Cron.runPropellor (Cron.Times "30 * * * *")
            & setupNode

setupNode :: Property (MetaTypes '[ 'WithInfo, 'Targeting 'OSDebian, 'Targeting 'OSBuntish])
setupNode =
    propertyList "Cardano node" $
        props
            & User.accountFor curry
            & Ssh.installed
            & Ssh.authorizedKeys curry hostContext
            & Git.pulled curry "https://github.com/input-output-hk/cardano-configurations" "cardano-configurations" Nothing
            & check
                shouldDownload
                ( cmdProperty
                    "curl"
                    ["-o", "/home/curry/cardanode-node-1.35.5.tgz", "-L", "https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.5-linux.tar.gz"]
                    `changesFileContent` "/home/curry/cardanode-node-1.35.5.tgz"
                )
            & File.ownerGroup "/home/curry/cardanode-node-1.35.5.tgz" curry curryGrp
  where
    sha256 = "bb9e9c3700ebdef4de3e34e5087a79dc30d27ca3c1c66af25957f9205dfe05aa"
    shouldDownload = liftPropellor $ do
        hasFile <- doesFileExist "/home/curry/cardanode-node-1.35.5.tgz"
        sha <- head . words . head . lines <$> readProcess "sha256sum" ["/home/curry/cardanode-node-1.35.5.tgz"]
        pure $ not hasFile || sha /= sha256


    curry = User "curry"
    curryGrp = Group "curry"
