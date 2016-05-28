module Propellor.Bootstrap (
	bootstrapPropellorCommand,
	checkBinaryCommand,
	installGitCommand,
	buildPropellor,
) where

import           Propellor.Base
import           Propellor.Types.Info

import           Data.List
import           System.Posix.Files

type ShellCommand = String

-- Shell command line to ensure propellor is bootstrapped and ready to run.
-- Should be run inside the propellor config dir, and will install
-- all necessary build dependencies and build propellor.
bootstrapPropellorCommand :: Maybe System -> ShellCommand
bootstrapPropellorCommand msys = checkDepsCommand msys ++
	"&& if ! test -x ./propellor; then "
		++ buildCommand ++
	"; fi;" ++ checkBinaryCommand

-- Use propellor --check to detect if the local propellor binary has
-- stopped working (eg due to library changes), and must be rebuilt.
checkBinaryCommand :: ShellCommand
checkBinaryCommand = "if test -x ./propellor && ! ./propellor --check; then " ++ go ++ "; fi"
  where
	go = intercalate " && "
		[ "/usr/local/bin/stack clean"
		, buildCommand
		]

buildCommand :: ShellCommand
buildCommand = intercalate " && "
	[ "/usr/local/bin/stack setup"
	, "/usr/local/bin/stack build propellor-config"
	, "ln -sf $(/usr/local/bin/stack path --dist-dir)/build/propellor-config propellor"
	]

-- Run cabal configure to check if all dependencies are installed;
-- if not, run the depsCommand.
checkDepsCommand :: Maybe System -> ShellCommand
checkDepsCommand sys = "if ! [ -x /usr/local/bin/stack ] ; then " ++ depsCommand sys ++ "; fi"

-- Install build dependencies of propellor, mainly stack
--
-- http://docs.haskellstack.org/en/stable/install_and_upgrade/#linux
depsCommand :: Maybe System -> ShellCommand
depsCommand msys = "( " ++ intercalate " ; " (concat [osinstall, stackinstall]) ++ " ) || true"
  where
	osinstall = case msys of
		Just (System (FreeBSD _) _) -> map pkginstall fbsddeps
		_                           -> useapt

        useapt = "apt-get update" : map aptinstall debdeps

        stackinstall =
		[ "wget -O stack.tgz https://www.stackage.org/stack/linux-x86_64"
		, "tar xf stack.tgz"
                  -- to find the path to stack exe... This is cumbersome because the downloaded archive's path containers
                  -- version number which we don't know...
		, "ln -s $(pwd)\"/\"$(find . -name stack) /usr/local/bin/stack"
		]

	aptinstall p = "DEBIAN_FRONTEND=noninteractive apt-get --no-upgrade --no-install-recommends -y install " ++ p
	pkginstall p = "ASSUME_ALWAYS_YES=yes pkg install " ++ p

	-- This is the same deps listed in debian/control.
	debdeps =
		[ "wget" ]
	fbsddeps =
		[ "devel/gmake"
                , "perl5"
                , "lang/gcc"
                , "misc/compat8x"
                , "misc/compat9x"
                , "converters/libiconv"
                , "ca_root_nss"
                , "wget"
		]

installGitCommand :: Maybe System -> ShellCommand
installGitCommand msys = case msys of
	(Just (System (Debian _) _)) -> use apt
	(Just (System (Buntish _) _)) -> use apt
	(Just (System (FreeBSD _) _)) -> use
		[ "ASSUME_ALWAYS_YES=yes pkg update"
		, "ASSUME_ALWAYS_YES=yes pkg install git"
		]
	-- assume a debian derived system when not specified
	Nothing -> use apt
  where
	use cmds = "if ! git --version >/dev/null; then " ++ intercalate " && " cmds ++ "; fi"
	apt =
		[ "apt-get update"
		, "DEBIAN_FRONTEND=noninteractive apt-get --no-install-recommends --no-upgrade -y install git"
		]

buildPropellor :: Maybe Host -> IO ()
buildPropellor mh = unlessM (actionMessage "Propellor build" (build msys)) $
	errorMessage "Propellor build failed!"
  where
	msys = case fmap (getInfo . hostInfo) mh of
		Just (InfoVal sys) -> Just sys
		_ -> Nothing

-- Build propellor using stack, and symlink propellor to where stack
-- leaves the built binary.
--
build :: Maybe System -> IO Bool
build msys = catchBoolIO $ do
	unlessM stack_build $ error "stack build failed"
        (stackDistPath,succeed) <- processTranscript "/usr/local/bin/stack" ["path", "--dist-dir"] Nothing
        when (not succeed) $ error "stack failed to extract path to executable"
	-- For safety against eg power loss in the middle of the build,
	-- make a copy of the binary, and move it into place atomically.
	-- This ensures that the propellor symlink only ever points at
	-- a binary that is fully built. Also, avoid ever removing
	-- or breaking the symlink.
	--
	-- Need cp -a to make build timestamp checking work.
	unlessM (boolSystem "cp" [Param "-af", Param (stackDistPath </> stackbuiltbin), Param (tmpfor safetycopy)]) $
		error "cp of binary failed"
	rename (tmpfor safetycopy) safetycopy
	createSymbolicLink safetycopy (tmpfor dest)
	rename (tmpfor dest) dest
	return True
  where
	dest = "propellor"
	stackbuiltbin = "propellor-config"
	safetycopy = stackbuiltbin ++ ".built"
	tmpfor f = f ++ ".propellortmp"
	stack_build = stack ["build", "propellor-config"]

stack :: [String] -> IO Bool
stack = boolSystem "/usr/local/bin/stack" . map Param
