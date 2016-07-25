module Propellor.Property.HostingProvider.Linode where

import Propellor.Base
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File
import Utility.FileMode

-- | Linode's pv-grub-x86_64 does not currently support booting recent
-- Debian kernels compressed with xz. This sets up pv-grub chaining to enable
-- it.
chainPVGrub :: Grub.TimeoutSecs -> Property DebianLike
chainPVGrub = Grub.chainPVGrub "hd0" "xen/xvda"

-- | Linode disables mlocate's cron job's execute permissions,
-- presumably to avoid disk IO. This ensures it's executable.
mlocateEnabled :: Property DebianLike
mlocateEnabled = tightenTargets $
	"/etc/cron.daily/mlocate"
		`File.mode` combineModes (readModes ++ executeModes)

