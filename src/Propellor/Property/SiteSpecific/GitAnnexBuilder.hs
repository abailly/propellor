{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.SiteSpecific.GitAnnexBuilder where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Cron as Cron
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Systemd as Systemd
import qualified Propellor.Property.Chroot as Chroot
import Propellor.Property.Cron (Times)

builduser :: UserName
builduser = "builder"

homedir :: FilePath
homedir = "/home/builder"

gitbuilderdir :: FilePath
gitbuilderdir = homedir </> "gitbuilder"

builddir :: FilePath
builddir = gitbuilderdir </> "build"

type TimeOut = String -- eg, 5h

type ArchString = String

autobuilder :: ArchString -> Times -> TimeOut -> Property (HasInfo + DebianLike)
autobuilder arch crontimes timeout = combineProperties "gitannexbuilder" $ props
	& Apt.serviceInstalledRunning "cron"
	& Cron.niceJob "gitannexbuilder" crontimes (User builduser) gitbuilderdir
		("git pull ; timeout " ++ timeout ++ " ./autobuild")
	& rsyncpassword
  where
	context = Context ("gitannexbuilder " ++ arch)
	pwfile = homedir </> "rsyncpassword"
	-- The builduser account does not have a password set,
	-- instead use the password privdata to hold the rsync server
	-- password used to upload the built image.
	rsyncpassword :: Property (HasInfo + DebianLike)
	rsyncpassword = withPrivData (Password builduser) context $ \getpw ->
		property "rsync password" $ getpw $ \pw -> do
			have <- liftIO $ catchDefaultIO "" $
				readFileStrict pwfile
			let want = privDataVal pw
			if want /= have
				then makeChange $ writeFile pwfile want
				else noChange

tree :: ArchString -> Flavor -> Property DebianLike
tree buildarch flavor = combineProperties "gitannexbuilder tree" $ props
	& Apt.installed ["git"]
	& File.dirExists gitbuilderdir
	& File.ownerGroup gitbuilderdir (User builduser) (Group builduser)
	& gitannexbuildercloned
	& builddircloned
  where
	gitannexbuildercloned = check (not <$> (doesDirectoryExist (gitbuilderdir </> ".git"))) $
		userScriptProperty (User builduser)
			[ "git clone git://git.kitenet.net/gitannexbuilder " ++ gitbuilderdir
			, "cd " ++ gitbuilderdir
			, "git checkout " ++ buildarch ++ fromMaybe "" flavor
			]
			`assume` MadeChange
			`describe` "gitbuilder setup"
	builddircloned = check (not <$> doesDirectoryExist builddir) $ userScriptProperty (User builduser)
		[ "git clone git://git-annex.branchable.com/ " ++ builddir
		]

buildDepsApt :: Property DebianLike
buildDepsApt = combineProperties "gitannexbuilder build deps" $ props
	& Apt.buildDep ["git-annex"]
	& buildDepsNoHaskellLibs
	& Apt.buildDepIn builddir
		`describe` "git-annex source build deps installed"

buildDepsNoHaskellLibs :: Property DebianLike
buildDepsNoHaskellLibs = Apt.installed
	["git", "rsync", "moreutils", "ca-certificates",
	"debhelper", "ghc", "curl", "openssh-client", "git-remote-gcrypt",
	"liblockfile-simple-perl", "cabal-install", "vim", "less",
	-- needed by haskell libs
	"libxml2-dev", "libidn11-dev", "libgsasl7-dev", "libgnutls28-dev",
	"libmagic-dev", "alex", "happy", "c2hs"
	]

haskellPkgsInstalled :: String -> Property DebianLike
haskellPkgsInstalled dir = tightenTargets $
	flagFile go ("/haskellpkgsinstalled")
  where
	go = userScriptProperty (User builduser)
		[ "cd " ++ builddir ++ " && ./standalone/" ++ dir ++ "/install-haskell-packages"
		]
		`assume` MadeChange

-- Installs current versions of git-annex's deps from cabal, but only
-- does so once.
cabalDeps :: Property UnixLike
cabalDeps = flagFile go cabalupdated
	where
		go = userScriptProperty (User builduser)
			["cabal update && cabal install git-annex --only-dependencies || true"]
			`assume` MadeChange
		cabalupdated = homedir </> ".cabal" </> "packages" </> "hackage.haskell.org" </> "00-index.cache"

autoBuilderContainer :: (DebianSuite -> Architecture -> Flavor -> Property (HasInfo + Debian)) -> DebianSuite -> Architecture -> Flavor -> Times -> TimeOut -> Systemd.Container
autoBuilderContainer mkprop suite arch flavor crontime timeout =
	Systemd.container name $ \d -> Chroot.debootstrapped mempty d $ props
		& mkprop suite arch flavor
		& autobuilder (architectureToDebianArchString arch) crontime timeout
  where
	name = architectureToDebianArchString arch ++ fromMaybe "" flavor ++ "-git-annex-builder"

type Flavor = Maybe String

standardAutoBuilder :: DebianSuite -> Architecture -> Flavor -> Property (HasInfo + Debian)
standardAutoBuilder suite arch flavor =
	propertyList "standard git-annex autobuilder" $ props
		& osDebian suite arch
		& buildDepsApt
		& Apt.stdSourcesList
		& Apt.unattendedUpgrades
		& Apt.cacheCleaned
		& User.accountFor (User builduser)
		& tree (architectureToDebianArchString arch) flavor

stackAutoBuilder :: DebianSuite -> Architecture -> Flavor -> Property (HasInfo + Debian)
stackAutoBuilder suite arch flavor =
	propertyList "git-annex autobuilder using stack" $ props
		& osDebian suite arch
		& buildDepsNoHaskellLibs
		& Apt.stdSourcesList
		& Apt.unattendedUpgrades
		& Apt.cacheCleaned
		& User.accountFor (User builduser)
		& tree (architectureToDebianArchString arch) flavor
		& stackInstalled
		-- Workaround https://github.com/commercialhaskell/stack/issues/2093
		& Apt.installed ["libtinfo-dev"]

stackInstalled :: Property Linux
stackInstalled = withOS "stack installed" $ \w o ->
	case o of
		(Just (System (Debian Linux (Stable "jessie")) X86_32)) ->
			ensureProperty w $ manualinstall X86_32
		_ -> ensureProperty w $ Apt.installed ["haskell-stack"]
  where
	-- Warning: Using a binary downloaded w/o validation.
	manualinstall :: Architecture -> Property Linux
	manualinstall arch = tightenTargets $ check (not <$> doesFileExist binstack) $
		propertyList "stack installed from upstream tarball" $ props
			& cmdProperty "wget" ["https://www.stackage.org/stack/linux-" ++ architectureToDebianArchString arch, "-O", tmptar]
				`assume` MadeChange
			& File.dirExists tmpdir
			& cmdProperty "tar" ["xf", tmptar, "-C", tmpdir, "--strip-components=1"]
				`assume` MadeChange
			& cmdProperty "mv" [tmpdir </> "stack", binstack]
				`assume` MadeChange
			& cmdProperty "rm" ["-rf", tmpdir, tmptar]
				`assume` MadeChange
	binstack = "/usr/bin/stack"
	tmptar = "/root/stack.tar.gz"
	tmpdir = "/root/stack"

armAutoBuilder :: DebianSuite -> Architecture -> Flavor -> Property (HasInfo + Debian)
armAutoBuilder suite arch flavor =
	propertyList "arm git-annex autobuilder" $ props
		& standardAutoBuilder suite arch flavor
		& buildDepsNoHaskellLibs
		-- Works around ghc crash with parallel builds on arm.
		& (homedir </> ".cabal" </> "config")
			`File.lacksLine` "jobs: $ncpus"
		-- Install patched haskell packages for portability to
		-- arm NAS's using old kernel versions.
		& haskellPkgsInstalled "linux"

androidAutoBuilderContainer :: Times -> TimeOut -> Systemd.Container
androidAutoBuilderContainer crontimes timeout =
	androidAutoBuilderContainer' "android-git-annex-builder"
		(tree "android" Nothing) builddir crontimes timeout

-- Android is cross-built in a Debian i386 container, using the Android NDK.
androidAutoBuilderContainer'
	:: Systemd.MachineName
	-> Property DebianLike
	-> FilePath
	-> Times
	-> TimeOut
	-> Systemd.Container
androidAutoBuilderContainer' name setupgitannexdir gitannexdir crontimes timeout =
	Systemd.container name $ \d -> bootstrap d $ props
		& osDebian (Stable "jessie") X86_32
		& Apt.stdSourcesList
		& User.accountFor (User builduser)
		& File.dirExists gitbuilderdir
		& File.ownerGroup homedir (User builduser) (Group builduser)
		& flagFile chrootsetup ("/chrootsetup")
			`requires` setupgitannexdir
		& haskellPkgsInstalled "android"
		& Apt.unattendedUpgrades
		& buildDepsNoHaskellLibs
		& autobuilder "android" crontimes timeout
  where
	-- Use git-annex's android chroot setup script, which will install
	-- ghc-android and the NDK, all build deps, etc, in the home
	-- directory of the builder user.
	chrootsetup = scriptProperty
		[ "cd " ++ gitannexdir ++ " && ./standalone/android/buildchroot-inchroot"
		]
		`assume` MadeChange
	bootstrap = Chroot.debootstrapped mempty
