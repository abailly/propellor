{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Apt where

import Data.Maybe
import Data.List
import System.IO
import Control.Monad
import Control.Applicative
import Prelude

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Service as Service
import Propellor.Property.File (Line)

sourcesList :: FilePath
sourcesList = "/etc/apt/sources.list"

type Url = String
type Section = String

type SourcesGenerator = DebianSuite -> [Line]

showSuite :: DebianSuite -> String
showSuite (Stable s) = s
showSuite Testing = "testing"
showSuite Unstable = "unstable"
showSuite Experimental = "experimental"

backportSuite :: DebianSuite -> Maybe String
backportSuite (Stable s) = Just (s ++ "-backports")
backportSuite _ = Nothing

stableUpdatesSuite :: DebianSuite -> Maybe String
stableUpdatesSuite (Stable s) = Just (s ++ "-updates")
stableUpdatesSuite _ = Nothing

debLine :: String -> Url -> [Section] -> Line
debLine suite mirror sections = unwords $
	["deb", mirror, suite] ++ sections

srcLine :: Line -> Line
srcLine l = case words l of
	("deb":rest) -> unwords $ "deb-src" : rest
	_ -> ""

stdSections :: [Section]
stdSections = ["main", "contrib", "non-free"]

binandsrc :: String -> SourcesGenerator
binandsrc url suite = catMaybes
	[ Just l
	, Just $ srcLine l
	, bl
	, srcLine <$> bl
	]
  where
	l = debLine (showSuite suite) url stdSections
	bl = do
		bs <- backportSuite suite
		return $ debLine bs url stdSections

debCdn :: SourcesGenerator
debCdn = binandsrc "http://httpredir.debian.org/debian"

kernelOrg :: SourcesGenerator
kernelOrg = binandsrc "http://mirrors.kernel.org/debian"

-- | Only available for Stable and Testing
securityUpdates :: SourcesGenerator
securityUpdates suite
	| isStable suite || suite == Testing =
		let l = "deb http://security.debian.org/ " ++ showSuite suite ++ "/updates " ++ unwords stdSections
		in [l, srcLine l]
	| otherwise = []

-- | Makes sources.list have a standard content using the Debian mirror CDN,
-- with the Debian suite configured by the os.
--
-- Since the CDN is sometimes unreliable, also adds backup lines using
-- kernel.org.
stdSourcesList :: Property Debian
stdSourcesList = withOS "standard sources.list" $ \w o -> case o of
	(Just (System (Debian _ suite) _)) ->
		ensureProperty w $ stdSourcesListFor suite
	_ -> unsupportedOS'

stdSourcesListFor :: DebianSuite -> Property Debian
stdSourcesListFor suite = stdSourcesList' suite []

-- | Adds additional sources.list generators.
--
-- Note that if a Property needs to enable an apt source, it's better
-- to do so via a separate file in </etc/apt/sources.list.d/>
stdSourcesList' :: DebianSuite -> [SourcesGenerator] -> Property Debian
stdSourcesList' suite more = tightenTargets $ setSourcesList
	(concatMap (\gen -> gen suite) generators)
	`describe` ("standard sources.list for " ++ show suite)
  where
	generators = [debCdn, kernelOrg, securityUpdates] ++ more

setSourcesList :: [Line] -> Property DebianLike
setSourcesList ls = sourcesList `File.hasContent` ls `onChange` update

setSourcesListD :: [Line] -> FilePath -> Property DebianLike
setSourcesListD ls basename = f `File.hasContent` ls `onChange` update
  where
	f = "/etc/apt/sources.list.d/" ++ basename ++ ".list"

runApt :: [String] -> UncheckedProperty DebianLike
runApt ps = tightenTargets $ cmdPropertyEnv "apt-get" ps noninteractiveEnv

noninteractiveEnv :: [(String, String)]
noninteractiveEnv =
		[ ("DEBIAN_FRONTEND", "noninteractive")
		, ("APT_LISTCHANGES_FRONTEND", "none")
		]

-- | Have apt update its lists of packages, but without upgrading anything.
update :: Property DebianLike
update = combineProperties ("apt update") $ props
	& pendingConfigured
	& runApt ["update"]
		`assume` MadeChange

-- | Have apt upgrade packages, adding new packages and removing old as
-- necessary. Often used in combination with the `update` property.
upgrade :: Property DebianLike
upgrade = upgrade' "dist-upgrade"

upgrade' :: String -> Property DebianLike
upgrade' p = combineProperties ("apt " ++ p) $ props
	& pendingConfigured
	& runApt ["-y", p]
		`assume` MadeChange

-- | Have apt upgrade packages, but never add new packages or remove
-- old packages. Not suitable for upgrading acrocess major versions
-- of the distribution.
safeUpgrade :: Property DebianLike
safeUpgrade = upgrade' "upgrade"

-- | Have dpkg try to configure any packages that are not fully configured.
pendingConfigured :: Property DebianLike
pendingConfigured = tightenTargets $
	cmdPropertyEnv "dpkg" ["--configure", "--pending"] noninteractiveEnv
		`assume` MadeChange
		`describe` "dpkg configured pending"

type Package = String

installed :: [Package] -> Property DebianLike
installed = installed' ["-y"]

installed' :: [String] -> [Package] -> Property DebianLike
installed' params ps = robustly $ check (not <$> isInstalled' ps) go
	`describe` unwords ("apt installed":ps)
  where
	go = runApt (params ++ ["install"] ++ ps)

installedBackport :: [Package] -> Property Debian
installedBackport ps = withOS desc $ \w o -> case o of
	(Just (System (Debian _ suite) _)) -> case backportSuite suite of
		Nothing -> unsupportedOS'
		Just bs -> ensureProperty w $
			runApt (["install", "-t", bs, "-y"] ++ ps)
				`changesFile` dpkgStatus
	_ -> unsupportedOS'
  where
	desc = unwords ("apt installed backport":ps)

-- | Minimal install of package, without recommends.
installedMin :: [Package] -> Property DebianLike
installedMin = installed' ["--no-install-recommends", "-y"]

removed :: [Package] -> Property DebianLike
removed ps = check (any (== IsInstalled) <$> getInstallStatus ps)
	(runApt (["-y", "remove"] ++ ps))
	`describe` unwords ("apt removed":ps)

buildDep :: [Package] -> Property DebianLike
buildDep ps = robustly $ go
	`changesFile` dpkgStatus
	`describe` unwords ("apt build-dep":ps)
  where
	go = runApt $ ["-y", "build-dep"] ++ ps

-- | Installs the build deps for the source package unpacked
-- in the specifed directory, with a dummy package also
-- installed so that autoRemove won't remove them.
buildDepIn :: FilePath -> Property DebianLike
buildDepIn dir = cmdPropertyEnv "sh" ["-c", cmd] noninteractiveEnv
	`changesFile` dpkgStatus
	`requires` installedMin ["devscripts", "equivs"]
  where
	cmd = "cd '" ++ dir ++ "' && mk-build-deps debian/control --install --tool 'apt-get -y --no-install-recommends' --remove"

-- | Package installation may fail becuse the archive has changed.
-- Run an update in that case and retry.
robustly :: Property DebianLike -> Property DebianLike
robustly p = p `fallback` (update `before` p)

isInstalled :: Package -> IO Bool
isInstalled p = isInstalled' [p]

isInstalled' :: [Package] -> IO Bool
isInstalled' ps = all (== IsInstalled) <$> getInstallStatus ps

data InstallStatus = IsInstalled | NotInstalled
	deriving (Show, Eq)

{- Returns the InstallStatus of packages that are installed
 - or known and not installed. If a package is not known at all to apt
 - or dpkg, it is not included in the list. -}
getInstallStatus :: [Package] -> IO [InstallStatus]
getInstallStatus ps = mapMaybe parse . lines <$> policy
  where
	parse l
		| "Installed: (none)" `isInfixOf` l = Just NotInstalled
		| "Installed: " `isInfixOf` l = Just IsInstalled
		| otherwise = Nothing
	policy = do
		environ <- addEntry "LANG" "C" <$> getEnvironment
		readProcessEnv "apt-cache" ("policy":ps) (Just environ)

autoRemove :: Property DebianLike
autoRemove = runApt ["-y", "autoremove"]
	`changesFile` dpkgStatus
	`describe` "apt autoremove"

-- | Enables unattended upgrades. Revert to disable.
unattendedUpgrades :: RevertableProperty DebianLike DebianLike
unattendedUpgrades = enable <!> disable
  where
	enable = setup True
		`before` Service.running "cron"
		`before` configure
		-- work around http://bugs.debian.org/812380
		`before` File.notPresent "/etc/apt/apt.conf.d/50unattended-upgrades.ucf-dist"
	disable = setup False

	setup enabled = (if enabled then installed else removed) ["unattended-upgrades"]
		`onChange` reConfigure "unattended-upgrades"
			[("unattended-upgrades/enable_auto_updates" , "boolean", v)]
		`describe` ("unattended upgrades " ++ v)
	  where
		v
			| enabled = "true"
			| otherwise = "false"

	configure :: Property DebianLike
	configure = propertyList "unattended upgrades configured" $ props
		& enableupgrading
		& unattendedconfig `File.containsLine` "Unattended-Upgrade::Mail \"root\";"
	  where
		enableupgrading :: Property DebianLike
		enableupgrading = withOS "unattended upgrades configured" $ \w o ->
			case o of
				-- the package defaults to only upgrading stable
				(Just (System (Debian _ suite) _))
					| not (isStable suite) -> ensureProperty w $
						unattendedconfig
							`File.containsLine`
						("Unattended-Upgrade::Origins-Pattern { \"o=Debian,a="++showSuite suite++"\"; };")
				_ -> noChange
		unattendedconfig = "/etc/apt/apt.conf.d/50unattended-upgrades"

-- | Enable periodic updates (but not upgrades), including download
-- of packages.
periodicUpdates :: Property DebianLike
periodicUpdates = tightenTargets $ "/etc/apt/apt.conf.d/02periodic" `File.hasContent`
	[ "APT::Periodic::Enable \"1\";"
	, "APT::Periodic::Update-Package-Lists \"1\";"
	, "APT::Periodic::Download-Upgradeable-Packages \"1\";"
	, "APT::Periodic::Verbose \"1\";"
	]

type DebconfTemplate = String
type DebconfTemplateType = String
type DebconfTemplateValue = String

-- | Preseeds debconf values and reconfigures the package so it takes
-- effect.
reConfigure :: Package -> [(DebconfTemplate, DebconfTemplateType, DebconfTemplateValue)] -> Property DebianLike
reConfigure package vals = tightenTargets $
	reconfigure
		`requires` setselections
		`describe` ("reconfigure " ++ package)
  where
	setselections :: Property DebianLike
	setselections = property "preseed" $
		if null vals
			then noChange
			else makeChange $
				withHandle StdinHandle createProcessSuccess
					(proc "debconf-set-selections" []) $ \h -> do
						forM_ vals $ \(tmpl, tmpltype, value) ->
							hPutStrLn h $ unwords [package, tmpl, tmpltype, value]
						hClose h
	reconfigure = cmdPropertyEnv "dpkg-reconfigure" ["-fnone", package] noninteractiveEnv
		`assume` MadeChange

-- | Ensures that a service is installed and running.
--
-- Assumes that there is a 1:1 mapping between service names and apt
-- package names.
serviceInstalledRunning :: Package -> Property DebianLike
serviceInstalledRunning svc = Service.running svc `requires` installed [svc]

data AptKey = AptKey
	{ keyname :: String
	, pubkey :: String
	}

trustsKey :: AptKey -> RevertableProperty DebianLike DebianLike
trustsKey k = trustsKey' k <!> untrustKey k

trustsKey' :: AptKey -> Property DebianLike
trustsKey' k = check (not <$> doesFileExist f) $ property desc $ makeChange $ do
	withHandle StdinHandle createProcessSuccess
		(proc "gpg" ["--no-default-keyring", "--keyring", f, "--import", "-"]) $ \h -> do
			hPutStr h (pubkey k)
			hClose h
	nukeFile $ f ++ "~" -- gpg dropping
  where
	desc = "apt trusts key " ++ keyname k
	f = aptKeyFile k

untrustKey :: AptKey -> Property DebianLike
untrustKey = tightenTargets . File.notPresent . aptKeyFile

aptKeyFile :: AptKey -> FilePath
aptKeyFile k = "/etc/apt/trusted.gpg.d" </> keyname k ++ ".gpg"

-- | Cleans apt's cache of downloaded packages to avoid using up disk
-- space.
cacheCleaned :: Property DebianLike
cacheCleaned = tightenTargets $ cmdProperty "apt-get" ["clean"]
	`assume` NoChange
	`describe` "apt cache cleaned"

-- | Add a foreign architecture to dpkg and apt.
hasForeignArch :: String -> Property DebianLike
hasForeignArch arch = check notAdded (add `before` update)
	`describe` ("dpkg has foreign architecture " ++ arch)
  where
	notAdded = (notElem arch . lines) <$> readProcess "dpkg" ["--print-foreign-architectures"]
	add = cmdProperty "dpkg" ["--add-architecture", arch]
		`assume` MadeChange

dpkgStatus :: FilePath
dpkgStatus = "/var/lib/dpkg/status"
