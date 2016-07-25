module Propellor.Property.Grub where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt

-- | Eg, \"hd0,0\" or \"xen/xvda1\"
type GrubDevice = String

-- | Eg, \"\/dev/sda\"
type OSDevice = String

type TimeoutSecs = Int

-- | Types of machines that grub can boot.
data BIOS = PC | EFI64 | EFI32 | Coreboot | Xen

-- | Installs the grub package. This does not make grub be used as the
-- bootloader.
--
-- This includes running update-grub.
installed :: BIOS -> Property DebianLike
installed bios = installed' bios `onChange` mkConfig

-- Run update-grub, to generate the grub boot menu. It will be
-- automatically updated when kernel packages are installed.
mkConfig :: Property DebianLike
mkConfig = tightenTargets $ cmdProperty "update-grub" []
	`assume` MadeChange

-- | Installs grub; does not run update-grub.
installed' :: BIOS -> Property Linux
installed' bios = (aptinstall `pickOS` unsupportedOS)
	`describe` "grub package installed"
  where
	aptinstall :: Property DebianLike
	aptinstall = Apt.installed [debpkg]
	debpkg = case bios of
		PC -> "grub-pc"
		EFI64 -> "grub-efi-amd64"
		EFI32 -> "grub-efi-ia32"
		Coreboot -> "grub-coreboot"
		Xen -> "grub-xen"

-- | Installs grub onto a device, so the system can boot from that device.
--
-- You may want to install grub to multiple devices; eg for a system
-- that uses software RAID.
--
-- Note that this property does not check if grub is already installed
-- on the device; it always does the work to reinstall it. It's a good idea
-- to arrange for this property to only run once, by eg making it be run
-- onChange after OS.cleanInstallOnce.
boots :: OSDevice -> Property Linux
boots dev = tightenTargets $ cmdProperty "grub-install" [dev]
	`assume` MadeChange
	`describe` ("grub boots " ++ dev)

-- | Use PV-grub chaining to boot
--
-- Useful when the VPS's pv-grub is too old to boot a modern kernel image.
--
-- <http://notes.pault.ag/linode-pv-grub-chainning/>
--
-- The rootdev should be in the form "hd0", while the bootdev is in the form
-- "xen/xvda".
chainPVGrub :: GrubDevice -> GrubDevice -> TimeoutSecs -> Property DebianLike
chainPVGrub rootdev bootdev timeout = combineProperties desc $ props
	& File.dirExists "/boot/grub"
	& "/boot/grub/menu.lst" `File.hasContent`
		[ "default 1" 
		, "timeout " ++ show timeout
		, ""
		, "title grub-xen shim"
		, "root (" ++ rootdev ++ ")"
		, "kernel /boot/xen-shim"
		, "boot"
		]
	& "/boot/load.cf" `File.hasContent`
		[ "configfile (" ++ bootdev ++ ")/boot/grub/grub.cfg" ]
	& installed Xen
	& flip flagFile "/boot/xen-shim" xenshim
  where
	desc = "chain PV-grub"
	xenshim = scriptProperty ["grub-mkimage --prefix '(" ++ bootdev ++ ")/boot/grub' -c /boot/load.cf -O x86_64-xen /usr/lib/grub/x86_64-xen/*.mod > /boot/xen-shim"]
		`assume` MadeChange
		`describe` "/boot-xen-shim"
