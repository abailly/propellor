-- | Maintainer: Sean Whitton <spwhitton@spwhitton.name>

module Propellor.Property.LightDM where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.ConfFile as ConfFile

installed :: Property DebianLike
installed = Apt.installed ["lightdm"]

-- | Configures LightDM to skip the login screen and autologin as a user.
autoLogin :: User -> Property UnixLike
autoLogin (User u) = "/etc/lightdm/lightdm.conf" `ConfFile.containsIniSetting`
	("SeatDefaults", "autologin-user", u)
	`describe` "lightdm autologin"
