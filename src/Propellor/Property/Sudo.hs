module Propellor.Property.Sudo where

import Data.List

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.User

-- | Allows a user to sudo. If the user has a password, sudo is configured
-- to require it. If not, NOPASSWORD is enabled for the user.
enabledFor :: User -> Property DebianLike
enabledFor user@(User u) = go `requires` Apt.installed ["sudo"]
  where
	go :: Property UnixLike
	go = property' desc $ \w -> do
		locked <- liftIO $ isLockedPassword user
		ensureProperty w $
			fileProperty desc
				(modify locked . filter (wanted locked))
				"/etc/sudoers"
	desc = u ++ " is sudoer"
	sudobaseline = u ++ " ALL=(ALL:ALL)"
	sudoline True = sudobaseline ++ " NOPASSWD:ALL"
	sudoline False = sudobaseline ++ " ALL"
	wanted locked l
		-- TOOD: Full sudoers file format parse.. 
		| not (sudobaseline `isPrefixOf` l) = True
		| "NOPASSWD" `isInfixOf` l = locked
		| otherwise = True
	modify locked ls
		| sudoline locked `elem` ls = ls
		| otherwise = ls ++ [sudoline locked]
