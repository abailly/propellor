-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.User as User

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

setupNode :: Property DebianLike
setupNode =
    propertyList "Cardano node" $
        props
            & User.accountFor curry
            & Git.installed
            & Git.cloned curry "https://github.com/input-output-hk/cardano-configurations" "cardano-configurations" Nothing
  where
    curry = User "curry"
