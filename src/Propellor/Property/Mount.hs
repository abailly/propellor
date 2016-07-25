{-# LANGUAGE GeneralizedNewtypeDeriving, TypeSynonymInstances, FlexibleInstances #-}

-- | Properties in this module ensure that things are currently mounted,
-- but without making the mount persistent. Use `Propellor.Property.Fstab`
-- to configure persistent mounts.

module Propellor.Property.Mount where

import Propellor.Base
import Utility.Path

import Data.List

-- | type of filesystem to mount ("auto" to autodetect)
type FsType = String

-- | A device or other thing to be mounted.
type Source = String

-- | A mount point for a filesystem.
type MountPoint = FilePath

-- | Filesystem mount options. Eg, MountOpts ["errors=remount-ro"]
--
-- For default mount options, use `mempty`.
newtype MountOpts = MountOpts [String]
	deriving Monoid

class ToMountOpts a where
	toMountOpts :: a -> MountOpts
	
instance ToMountOpts MountOpts where
	toMountOpts = id

instance ToMountOpts String where
	toMountOpts s = MountOpts [s]

formatMountOpts :: MountOpts -> String
formatMountOpts (MountOpts []) = "defaults"
formatMountOpts (MountOpts l) = intercalate "," l

-- | Mounts a device, without listing it in </etc/fstab>.
mounted :: FsType -> Source -> MountPoint -> MountOpts -> Property UnixLike
mounted fs src mnt opts = property (mnt ++ " mounted") $ 
	toResult <$> liftIO (mount fs src mnt opts)

-- | Bind mounts the first directory so its contents also appear
-- in the second directory.
bindMount :: FilePath -> FilePath -> Property Linux
bindMount src dest = tightenTargets $
	cmdProperty "mount" ["--bind", src, dest]
		`assume` MadeChange
		`describe` ("bind mounted " ++ src ++ " to " ++ dest)

mount :: FsType -> Source -> MountPoint -> MountOpts -> IO Bool
mount fs src mnt opts = boolSystem "mount" $
	[ Param "-t", Param fs
	, Param "-o", Param (formatMountOpts opts)
	, Param src
	, Param mnt
	]

-- | Lists all mount points of the system.
mountPoints :: IO [MountPoint]
mountPoints = lines <$> readProcess "findmnt" ["-rn", "--output", "target"]

-- | Finds all filesystems mounted inside the specified directory.
mountPointsBelow :: FilePath -> IO [MountPoint]
mountPointsBelow target = filter (\p -> simplifyPath p /= simplifyPath target)
	. filter (dirContains target)
	<$> mountPoints

-- | Filesystem type mounted at a given location.
getFsType :: MountPoint -> IO (Maybe FsType)
getFsType = findmntField "fstype"

-- | Mount options for the filesystem mounted at a given location.
getFsMountOpts :: MountPoint -> IO MountOpts
getFsMountOpts p = maybe mempty toMountOpts
	<$> findmntField "fs-options" p

type UUID = String

-- | UUID of filesystem mounted at a given location.
getMountUUID :: MountPoint -> IO (Maybe UUID)
getMountUUID = findmntField "uuid"

-- | UUID of a device
getSourceUUID :: Source -> IO (Maybe UUID)
getSourceUUID = blkidTag "UUID"

type Label = String

-- | Label of filesystem mounted at a given location.
getMountLabel :: MountPoint -> IO (Maybe Label)
getMountLabel = findmntField "label"

-- | Label of a device
getSourceLabel :: Source -> IO (Maybe UUID)
getSourceLabel = blkidTag "LABEL"

-- | Device mounted at a given location.
getMountSource :: MountPoint -> IO (Maybe Source)
getMountSource = findmntField "source"

findmntField :: String -> FilePath -> IO (Maybe String)
findmntField field mnt = catchDefaultIO Nothing $
	headMaybe . filter (not . null) . lines
		<$> readProcess "findmnt" ["-n", mnt, "--output", field]

blkidTag :: String -> Source -> IO (Maybe String)
blkidTag tag dev = catchDefaultIO Nothing $
	headMaybe . filter (not . null) . lines
		<$> readProcess "blkid" [dev, "-s", tag, "-o", "value"]

-- | Unmounts a device or mountpoint,
-- lazily so any running processes don't block it.
umountLazy :: FilePath -> IO ()
umountLazy mnt =  
	unlessM (boolSystem "umount" [ Param "-l", Param mnt ]) $
		stopPropellorMessage $ "failed unmounting " ++ mnt

-- | Unmounts anything mounted inside the specified directory.
unmountBelow :: FilePath -> IO ()
unmountBelow d = do
	submnts <- mountPointsBelow d
	forM_ submnts umountLazy
