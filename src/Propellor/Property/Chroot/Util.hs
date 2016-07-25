module Propellor.Property.Chroot.Util where

import Propellor.Property.Mount

import Utility.Exception
import Utility.Env
import Utility.Directory

import Control.Applicative
import Prelude

-- | When chrooting, it's useful to ensure that PATH has all the standard
-- directories in it. This adds those directories to whatever PATH is
-- already set.
standardPathEnv :: IO [(String, String)]
standardPathEnv = do
	path <- getEnvDefault "PATH" "/bin"
	addEntry "PATH" (path ++ stdPATH)
		<$> getEnvironment

stdPATH :: String
stdPATH = "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"

-- | Removes the contents of a chroot. First, unmounts any filesystems
-- mounted within it.
removeChroot :: FilePath -> IO ()
removeChroot c = do
	unmountBelow c
	removeDirectoryRecursive c

-- | Returns true if a chroot directory is empty.
unpopulated :: FilePath -> IO Bool
unpopulated d = null <$> catchDefaultIO [] (dirContents d)
