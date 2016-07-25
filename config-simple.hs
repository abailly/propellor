-- This is the main configuration file for Propellor, and is used to build
-- the propellor program.

import Propellor
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.User as User

main :: IO ()
main = defaultMain hosts

-- The hosts propellor knows about.
hosts :: [Host]
hosts =
	[ mybox
	]

-- An example host.
mybox :: Host
mybox = host "mybox.example.com" $ props
	& osDebian Unstable X86_64
	& Apt.stdSourcesList
	& Apt.unattendedUpgrades
	& Apt.installed ["etckeeper"]
	& Apt.installed ["ssh"]
	& User.hasSomePassword (User "root")
	& File.dirExists "/var/www"
	& Cron.runPropellor (Cron.Times "30 * * * *")
