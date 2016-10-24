{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Maintainer: Sean Whitton <spwhitton@spwhitton.name>

Build and maintain schroots for use with sbuild.

For convenience we set up several enhancements, such as ccache and
eatmydata.  This means we have to make several assumptions:

1. you want to build for a Debian release strictly newer than squeeze,
or for a Buntish release newer than or equal to trusty

2. if you want to build for Debian stretch or newer, you have sbuild 0.70.0 or
newer (there is a backport to jessie)

The latter is due to the migration from GnuPG v1 to GnuPG v2.1 in
Debian stretch, which older sbuild can't handle.

Suggested usage in @config.hs@:

>  & Apt.installed ["piuparts", "autopkgtest"]
>  & Sbuild.builtFor (System (Debian Linux Unstable) X86_32) Sbuild.UseCcache
>  & Sbuild.piupartsConfFor (System (Debian Linux Unstable) X86_32)
>  & Sbuild.updatedFor (System (Debian Linux Unstable) X86_32) `period` Weekly 1
>  & Sbuild.usableBy (User "spwhitton")
>  & Sbuild.shareAptCache
>  & Schroot.overlaysInTmpfs

If you are using sbuild older than 0.70.0, you also need:

>  & Sbuild.keypairGenerated

In @~/.sbuildrc@ (sbuild 0.71.0 or newer):

>  $run_piuparts = 1;
>  $piuparts_opts = [
>      '--schroot',
>      '%r-%a-piuparts',
>      '--fail-if-inadequate',
>      '--fail-on-broken-symlinks',
>      ];
>
>  $run_autopkgtest = 1;
>  $autopkgtest_root_args = "";
>  $autopkgtest_opts = ["--", "schroot", "%r-%a-sbuild"];

We use @sbuild-createchroot(1)@ to create a chroot to the
specification of @sbuild-setup(7)@.  This avoids running propellor
inside the chroot to set it up.  While that approach is flexible, a
propellor spin pulls in a lot of dependencies.  This could defeat
using sbuild to determine if you've included all necessary build
dependencies in your source package control file.

Nevertheless, the chroot that @sbuild-createchroot(1)@ creates might
not meet your needs.  For example, you might need to enable an apt
cacher.  In that case you can do something like this in @config.hs@:

>  & Sbuild.built (System (Debian Linux Unstable) X86_32) `before` mySetup
>    where
>  	mySetup = Chroot.provisioned myChroot
>  	myChroot = Chroot.debootstrapped
>  		 	Debootstrap.BuilddD "/srv/chroot/unstable-i386"
>  		-- the extra configuration you need:
>  		& Apt.installed ["apt-transport-https"]
-}

-- Also see the --setup-only option of sbuild-createchroot

module Propellor.Property.Sbuild (
	-- * Creating and updating sbuild schroots
	SbuildSchroot(..),
	UseCcache(..),
	built,
	updated,
	piupartsConf,
	builtFor,
	updatedFor,
	piupartsConfFor,
	-- * Global sbuild configuration
	-- blockNetwork,
	installed,
	keypairGenerated,
	keypairInsecurelyGenerated,
	shareAptCache,
	usableBy,
) where

import Propellor.Base
import Propellor.Property.Debootstrap (extractSuite)
import Propellor.Property.Chroot.Util
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Ccache as Ccache
import qualified Propellor.Property.ConfFile as ConfFile
import qualified Propellor.Property.File as File
-- import qualified Propellor.Property.Firewall as Firewall
import qualified Propellor.Property.Schroot as Schroot
import qualified Propellor.Property.Reboot as Reboot
import qualified Propellor.Property.User as User

import Utility.FileMode
import Data.List
import Data.List.Utils

type Suite = String

-- | An sbuild schroot, such as would be listed by @schroot -l@
--
-- Parts of the sbuild toolchain cannot distinguish between schroots with both
-- the same suite and the same architecture, so neither do we
data SbuildSchroot = SbuildSchroot Suite Architecture

instance Show SbuildSchroot where
	show (SbuildSchroot suite arch) = suite ++ "-" ++ architectureToDebianArchString arch

-- | Whether an sbuild schroot should use ccache during builds
--
-- ccache is generally useful but it breaks building some packages.  This data
-- types allows you to toggle it on and off for particular schroots.
data UseCcache = UseCcache | NoCcache

-- | Build and configure a schroot for use with sbuild using a distribution's
-- standard mirror
--
-- This function is a convenience wrapper around 'built', allowing the user to
-- identify the schroot and distribution using the 'System' type
builtFor :: System -> UseCcache -> RevertableProperty DebianLike UnixLike
builtFor sys cc = go <!> deleted
  where
	go = property' ("sbuild schroot for " ++ show sys) $
		\w -> case (schrootFromSystem sys, stdMirror sys) of
			(Just s, Just u)  -> ensureProperty w $
				setupRevertableProperty $ built s u cc
			_ -> errorMessage
				("don't know how to debootstrap " ++ show sys)
	deleted = property' ("no sbuild schroot for " ++ show sys) $
		\w -> case schrootFromSystem sys of
			Just s  -> ensureProperty w $
				undoRevertableProperty $ built s "dummy" cc
			Nothing -> noChange

-- | Build and configure a schroot for use with sbuild
built :: SbuildSchroot -> Apt.Url -> UseCcache -> RevertableProperty DebianLike UnixLike
built s@(SbuildSchroot suite arch) mirror cc =
	((go `before` enhancedConf)
	`requires` ccacheMaybePrepared cc
	`requires` installed
	`requires` overlaysKernel)
	<!> deleted
  where
	go :: Property DebianLike
	go = check (unpopulated (schrootRoot s) <||> ispartial) $
		property' ("built sbuild schroot for " ++ show s) make
	make w = do
		de <- liftIO standardPathEnv
		let params = Param <$>
			[ "--arch=" ++ architectureToDebianArchString arch
			, "--chroot-suffix=-propellor"
			, "--include=eatmydata,ccache"
			, suite
			, schrootRoot s
			, mirror
			]
		ifM (liftIO $
			boolSystemEnv "sbuild-createchroot" params (Just de))
			( ensureProperty w $ fixConfFile s
			, return FailedChange
			)
	-- TODO we should kill any sessions still using the chroot
	-- before destroying it (as suggested by sbuild-destroychroot)
	deleted = check (not <$> unpopulated (schrootRoot s)) $
		property ("no sbuild schroot for " ++ show s) $ do
			liftIO $ removeChroot $ schrootRoot s
			liftIO $ nukeFile
				("/etc/sbuild/chroot" </> show s ++ "-sbuild")
			makeChange $ nukeFile (schrootConf s)

	enhancedConf =
		combineProperties ("enhanced schroot conf for " ++ show s) $ props
			& aliasesLine
			-- enable ccache and eatmydata for speed
			& ConfFile.containsIniSetting (schrootConf s)
				( show s ++ "-sbuild"
				, "command-prefix"
				, intercalate "," commandPrefix
				)

	-- if we're building a sid chroot, add useful aliases
	-- In order to avoid more than one schroot getting the same aliases, we
	-- only do this if the arch of the chroot equals the host arch.
	aliasesLine :: Property UnixLike
	aliasesLine = property' "maybe set aliases line" $ \w ->
		sidHostArchSchroot s >>= \isSidHostArchSchroot ->
			if isSidHostArchSchroot
			then ensureProperty w $
				ConfFile.containsIniSetting
					(schrootConf s)
					( show s ++ "-sbuild"
					, "aliases"
					, aliases
					)
			else return NoChange

	-- If the user has indicated that this host should use
	-- union-type=overlay schroots, we need to ensure that we have rebooted
	-- to a kernel supporting OverlayFS before we execute
	-- sbuild-setupchroot(1).  Otherwise, sbuild-setupchroot(1) will fail to
	-- add the union-type=overlay line to the schroot config.
	-- (We could just add that line ourselves, but then sbuild wouldn't work
	-- for the user, so we might as well do the reboot for them.)
	overlaysKernel :: Property DebianLike
	overlaysKernel = property' "reboot for union-type=overlay" $ \w ->
		Schroot.usesOverlays >>= \usesOverlays ->
			if usesOverlays
			then ensureProperty w $
				Reboot.toKernelNewerThan "3.18"
			else noChange

	-- A failed debootstrap run will leave a debootstrap directory;
	-- recover by deleting it and trying again.
	ispartial = ifM (doesDirectoryExist (schrootRoot s </> "debootstrap"))
		( do
			removeChroot $ schrootRoot s
			return True
		, return False
		)

	aliases = intercalate ","
		[ "sid"
		-- if the user wants to build for experimental, they would use
		-- their sid chroot and sbuild's --extra-repository option to
		-- enable experimental
		, "rc-buggy"
		, "experimental"
		-- we assume that building for UNRELEASED means building for
		-- unstable
		, "UNRELEASED"
		-- the following is for dgit compatibility:
		, "UNRELEASED-"
			++ architectureToDebianArchString arch
			++ "-sbuild"
		]

	commandPrefix = case cc of
		UseCcache -> "/var/cache/ccache-sbuild/sbuild-setup":base
		_ -> base
	  where
		base = ["eatmydata"]

-- | Ensure that an sbuild schroot's packages and apt indexes are updated
--
-- This function is a convenience wrapper around 'updated', allowing the user to
-- identify the schroot using the 'System' type
updatedFor :: System -> Property DebianLike
updatedFor system = property' ("updated sbuild schroot for " ++ show system) $
	\w -> case schrootFromSystem system of
		Just s  -> ensureProperty w $ updated s
		Nothing -> errorMessage
			("don't know how to debootstrap " ++ show system)

-- | Ensure that an sbuild schroot's packages and apt indexes are updated
updated :: SbuildSchroot -> Property DebianLike
updated s@(SbuildSchroot suite arch) =
	check (doesDirectoryExist (schrootRoot s)) $ go
	`describe` ("updated schroot for " ++ show s)
	`requires` installed
  where
	go :: Property DebianLike
	go = tightenTargets $ cmdProperty
		"sbuild-update" ["-udr", suite ++ "-" ++ architectureToDebianArchString arch]
		`assume` MadeChange

-- Find the conf file that sbuild-createchroot(1) made when we passed it
-- --chroot-suffix=propellor, and edit and rename such that it is as if we
-- passed --chroot-suffix=sbuild (the default).  Replace the random suffix with
-- 'propellor'.
--
-- We had to pass --chroot-suffix=propellor in order that we can find a unique
-- config file for the schroot we just built, despite the random suffix.
--
-- The properties in this module only permit the creation of one chroot for a
-- given suite and architecture, so we don't need the suffix to be random.
fixConfFile :: SbuildSchroot -> Property UnixLike
fixConfFile s@(SbuildSchroot suite arch) =
	property' ("schroot for " ++ show s ++ " config file fixed") $ \w -> do
		confs <- liftIO $ dirContents dir
		let old = concat $ filter (tempPrefix `isPrefixOf`) confs
		liftIO $ moveFile old new
		liftIO $ moveFile
			("/etc/sbuild/chroot" </> show s ++ "-propellor")
			("/etc/sbuild/chroot" </> show s ++ "-sbuild")
		ensureProperty w $
			File.fileProperty "replace dummy suffix" (map munge) new
  where
	new = schrootConf s
	dir = takeDirectory new
	tempPrefix = dir </> suite ++ "-" ++ architectureToDebianArchString arch ++ "-propellor-"
	munge = replace "-propellor]" "-sbuild]"

-- | Create a corresponding schroot config file for use with piuparts
--
-- This function is a convenience wrapper around 'piupartsConf', allowing the
-- user to identify the schroot using the 'System' type.  See that function's
-- documentation for why you might want to use this property, and sample config.
piupartsConfFor :: System -> Property DebianLike
piupartsConfFor sys = property' ("piuparts schroot conf for " ++ show sys) $
	\w -> case schrootFromSystem sys of
			Just s -> ensureProperty w $ piupartsConf s
			_ -> errorMessage
				("don't know how to debootstrap " ++ show sys)

-- | Create a corresponding schroot config file for use with piuparts
--
-- This is useful because:
--
-- - piuparts will clear out the apt cache which makes 'shareAptCache' much less
--   useful
--
-- - piuparts itself invokes eatmydata, so the command-prefix setting in our
--   regular schroot config would force the user to pass @--no-eatmydata@ to
--   piuparts in their @~/.sbuildrc@, which is inconvenient.
--
-- To make use of this new schroot config, you can put something like this in
-- your ~/.sbuildrc (sbuild 0.71.0 or newer):
--
--  >  $run_piuparts = 1;
--  >  $piuparts_opts = [
--  >      '--schroot',
--  >      '%r-%a-piuparts',
--  >      '--fail-if-inadequate',
--  >      '--fail-on-broken-symlinks',
--  >      ];
--
-- This property has no effect if the corresponding sbuild schroot does not
-- exist (i.e. you also need 'Sbuild.built' or 'Sbuild.builtFor').
piupartsConf :: SbuildSchroot -> Property DebianLike
piupartsConf s@(SbuildSchroot _ arch) =
	check (doesFileExist (schrootConf s)) go
	`requires` installed
  where
	go :: Property DebianLike
	go = property' desc $ \w -> do
		aliases <- aliasesLine
		ensureProperty w $ combineProperties desc $ props
			& check (not <$> doesFileExist f)
				(File.basedOn f (schrootConf s, map munge))
			& ConfFile.containsIniSetting f
				(sec, "profile", "piuparts")
			& ConfFile.containsIniSetting f
				(sec, "aliases", aliases)
			& ConfFile.containsIniSetting f
				(sec, "command-prefix", "")
			& File.dirExists dir
			& File.isSymlinkedTo (dir </> "copyfiles")
				(File.LinkTarget $ orig </> "copyfiles")
			& File.isSymlinkedTo (dir </> "nssdatabases")
				(File.LinkTarget $ orig </> "nssdatabases")
			& File.basedOn (dir </> "fstab")
				(orig </> "fstab", filter (/= aptCacheLine))

	orig = "/etc/schroot/sbuild"
	dir = "/etc/schroot/piuparts"
	sec = show s ++ "-piuparts"
	f = schrootPiupartsConf s
	munge = replace "-sbuild]" "-piuparts]"
	desc = "piuparts schroot conf for " ++ show s

	-- normally the piuparts schroot conf has no aliases, but we have to add
	-- one, for dgit compatibility, if this is the default sid chroot
	aliasesLine = sidHostArchSchroot s >>= \isSidHostArchSchroot ->
			return $ if isSidHostArchSchroot
			then "UNRELEASED-"
				++ architectureToDebianArchString arch
				++ "-piuparts"
			else ""

-- | Bind-mount /var/cache/apt/archives in all sbuild chroots so that the host
-- system and the chroot share the apt cache
--
-- This speeds up builds by avoiding unnecessary downloads of build
-- dependencies.
shareAptCache :: Property DebianLike
shareAptCache = File.containsLine "/etc/schroot/sbuild/fstab" aptCacheLine
	`requires` installed
	`describe` "sbuild schroots share host apt cache"

aptCacheLine :: String
aptCacheLine = "/var/cache/apt/archives /var/cache/apt/archives none rw,bind 0 0"

-- | Ensure that sbuild is installed
installed :: Property DebianLike
installed = Apt.installed ["sbuild"]

-- | Add an user to the sbuild group in order to use sbuild
usableBy :: User -> Property DebianLike
usableBy u = User.hasGroup u (Group "sbuild") `requires` installed

-- | Generate the apt keys needed by sbuild
--
-- You only need this if you are using sbuild older than 0.70.0.
keypairGenerated :: Property DebianLike
keypairGenerated = check (not <$> doesFileExist secKeyFile) $ go
	`requires` installed
	-- Work around Debian bug #792100 which is present in Jessie.
	-- Since this is a harmless mkdir, don't actually check the OS
	`requires` File.dirExists "/root/.gnupg"
  where
	go :: Property DebianLike
	go = tightenTargets $
		cmdProperty "sbuild-update" ["--keygen"]
		`assume` MadeChange

secKeyFile :: FilePath
secKeyFile = "/var/lib/sbuild/apt-keys/sbuild-key.sec"

-- | Generate the apt keys needed by sbuild using a low-quality source of
-- randomness
--
-- Note that any running rngd will be killed; if you are using rngd, you should
-- arrange for it to be restarted after this property has been ensured.  E.g.
--
-- >  & Sbuild.keypairInsecurelyGenerated
-- >  	`onChange` Systemd.started "my-rngd-service"
--
-- Useful on throwaway build VMs.
--
-- You only need this if you are using sbuild older than 0.70.0.
keypairInsecurelyGenerated :: Property DebianLike
keypairInsecurelyGenerated = check (not <$> doesFileExist secKeyFile) go
  where
	go :: Property DebianLike
	go = combineProperties "sbuild keyring insecurely generated" $ props
		& Apt.installed ["rng-tools"]
		-- If this dir does not exist the sbuild key generation command
		-- will fail; the user might have deleted it to work around
		-- #831462
		& File.dirExists "/var/lib/sbuild/apt-keys"
		-- If there is already an rngd process running we have to kill
		-- it, as it might not be feeding to /dev/urandom.  We can't
		-- kill by pid file because that is not guaranteed to be the
		-- default (/var/run/rngd.pid), so we killall
		& userScriptProperty (User "root")
			[ "start-stop-daemon -q -K -R 10 -o -n rngd"
			, "rngd -r /dev/urandom"
			]
			`assume` MadeChange
		& keypairGenerated
		-- Kill off the rngd process we spawned
		& userScriptProperty (User "root")
			["kill $(cat /var/run/rngd.pid)"]
			`assume` MadeChange

ccacheMaybePrepared :: UseCcache -> Property DebianLike
ccacheMaybePrepared cc = case cc of
	UseCcache -> ccachePrepared
	NoCcache  -> doNothing

-- another script from wiki.d.o/sbuild
ccachePrepared :: Property DebianLike
ccachePrepared = propertyList "sbuild group ccache configured" $ props
	-- We only set a limit on the cache if it doesn't already exist, so the
	-- user can override our default limit
	& check (not <$> doesDirectoryExist "/var/cache/ccache-sbuild")
		(Ccache.hasLimits "/var/cache/ccache-sbuild" (Ccache.MaxSize "2G"))
	`before` Ccache.hasCache (Group "sbuild") Ccache.NoLimit
	& "/etc/schroot/sbuild/fstab" `File.containsLine`
	"/var/cache/ccache-sbuild /var/cache/ccache-sbuild none rw,bind 0 0"
		`describe` "ccache mounted in sbuild schroots"
	& "/var/cache/ccache-sbuild/sbuild-setup" `File.hasContent`
		[ "#!/bin/sh"
		, ""
		, "export CCACHE_DIR=/var/cache/ccache-sbuild"
		, "export CCACHE_UMASK=002"
		, "export CCACHE_COMPRESS=1"
		, "unset CCACHE_HARDLINK"
		, "export PATH=\"/usr/lib/ccache:$PATH\""
		, ""
		, "exec \"$@\""
		]
	& File.mode "/var/cache/ccache-sbuild/sbuild-setup"
		(combineModes (readModes ++ executeModes))

-- This doesn't seem to work with the current version of sbuild
-- -- | Block network access during builds
-- --
-- -- This is a hack from <https://wiki.debian.org/sbuild> until #802850 and
-- -- #802849 are resolved.
-- blockNetwork :: Property Linux
-- blockNetwork = Firewall.rule Firewall.OUTPUT Firewall.Filter Firewall.DROP
-- 	(Firewall.GroupOwner (Group "sbuild")
-- 	<> Firewall.NotDestination
-- 		[Firewall.IPWithNumMask (IPv4 "127.0.0.1") 8])
-- 	`requires` installed 	-- sbuild group must exist

-- ==== utility functions ====

schrootFromSystem :: System -> Maybe SbuildSchroot
schrootFromSystem system@(System _ arch) =
	extractSuite system
	>>= \suite -> return $ SbuildSchroot suite arch

stdMirror :: System -> Maybe Apt.Url
stdMirror (System (Debian _ _) _) = Just "http://httpredir.debian.org/debian"
stdMirror (System (Buntish _) _) = Just "mirror://mirrors.ubuntu.com/"
stdMirror _ = Nothing

schrootRoot :: SbuildSchroot -> FilePath
schrootRoot (SbuildSchroot s a) = "/srv/chroot" </> s ++ "-" ++ architectureToDebianArchString a

schrootConf :: SbuildSchroot -> FilePath
schrootConf (SbuildSchroot s a) =
	"/etc/schroot/chroot.d" </> s ++ "-" ++ architectureToDebianArchString a ++ "-sbuild-propellor"

schrootPiupartsConf :: SbuildSchroot -> FilePath
schrootPiupartsConf (SbuildSchroot s a) =
	"/etc/schroot/chroot.d" </> s ++ "-" ++ architectureToDebianArchString a ++ "-piuparts-propellor"

-- Determine whether a schroot is
--
-- (i)  Debian sid, and
-- (ii) the same architecture as the host.
--
-- This is the "sid host arch schroot".  It is considered the default schroot
-- for sbuild builds, so we add useful aliases that work well with the suggested
-- ~/.sbuildrc given in the haddock
sidHostArchSchroot :: SbuildSchroot -> Propellor Bool
sidHostArchSchroot (SbuildSchroot suite arch) = do
	maybeOS <- getOS
	case maybeOS of
		Nothing -> return False
		Just (System _ hostArch) ->
			return $ suite == "unstable" && hostArch == arch
