module Propellor.Property.Fail2Ban where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import Propellor.Property.ConfFile

installed :: Property DebianLike
installed = Apt.serviceInstalledRunning "fail2ban"

reloaded :: Property DebianLike
reloaded = Service.reloaded "fail2ban"

type Jail = String

-- | By default, fail2ban only enables the ssh jail, but many others
-- are available to be enabled, for example "postfix-sasl"
jailEnabled :: Jail -> Property DebianLike
jailEnabled name = jailConfigured name "enabled" "true"
	`onChange` reloaded

-- | Configures a jail. For example:
--
-- > jailConfigured "sshd" "port" "2222"
jailConfigured :: Jail -> IniKey -> String -> Property UnixLike
jailConfigured name key value = 
	jailConfFile name `containsIniSetting` (name, key, value)

jailConfFile :: Jail -> FilePath
jailConfFile name = "/etc/fail2ban/jail.d/" ++ name ++ ".conf"
