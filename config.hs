-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.    https://propellor.branchable.com/

import Propellor
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Git as Git
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Ssh as Ssh

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

setupNode =
    propertyList "Cardano node" $
        props
            & User.accountFor curry
            & Ssh.installed
            & Ssh.userKeys
                curry
                hostContext
                [(SshEd25519,
                  "ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFEfXqphXP4Ui4YlgaojFQwfgYEkiZl2XRx3PlPypcMdQr6GhgxeLn4hNprCO49VvCajH/wINUJ6WlNpi8JLM5Y=")]
            & Git.installed
            & Git.pulled curry "https://github.com/input-output-hk/cardano-configurations" "cardano-configurations" Nothing
            & cmdProperty "curl"
            ["-O", "cardanode-node-1.35.5.tgz", "https://update-cardano-mainnet.iohk.io/cardano-node-releases/cardano-node-1.35.5-linux.tar.gz" ]
            `changesFile` "cardanode-node-1.35.5.tgz"

  where
    curry = User "curry"
