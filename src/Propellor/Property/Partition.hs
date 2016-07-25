{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Partition where

import Propellor.Base
import Propellor.Types.Core
import qualified Propellor.Property.Apt as Apt
import Utility.Applicative

import System.Posix.Files
import Data.List

-- | Filesystems etc that can be used for a partition.
data Fs = EXT2 | EXT3 | EXT4 | BTRFS | REISERFS | XFS | FAT | VFAT | NTFS | LinuxSwap
	deriving (Show, Eq)

data Eep = YesReallyFormatPartition

-- | Formats a partition.
formatted :: Eep -> Fs -> FilePath -> Property DebianLike
formatted = formatted' []

-- | Options passed to a mkfs.* command when making a filesystem.
--
-- Eg, ["-m0"]
type MkfsOpts = [String]

formatted' :: MkfsOpts -> Eep -> Fs -> FilePath -> Property DebianLike
formatted' opts YesReallyFormatPartition fs dev = cmdProperty cmd opts'
	`assume` MadeChange
	`requires` Apt.installed [pkg]
  where
	(cmd, opts', pkg) = case fs of
		EXT2 -> ("mkfs.ext2", q $ eff optsdev, "e2fsprogs")
		EXT3 -> ("mkfs.ext3", q $ eff optsdev, "e2fsprogs")
		EXT4 -> ("mkfs.ext4", q $ eff optsdev, "e2fsprogs")
		BTRFS -> ("mkfs.btrfs", optsdev, "btrfs-tools")
		REISERFS -> ("mkfs.reiserfs", q $ "-ff":optsdev, "reiserfsprogs")
		XFS -> ("mkfs.xfs", "-f":q optsdev, "xfsprogs")
		FAT -> ("mkfs.fat", optsdev, "dosfstools")
		VFAT -> ("mkfs.vfat", optsdev, "dosfstools")
		NTFS -> ("mkfs.ntfs", q $ eff optsdev, "ntfs-3g")
		LinuxSwap -> ("mkswap", optsdev, "util-linux")
	optsdev = opts++[dev]
	-- -F forces creating a filesystem even if the device already has one
	eff l = "-F":l
	-- Be quiet.
	q l = "-q":l

data LoopDev = LoopDev
	{ partitionLoopDev :: FilePath -- ^ device for a loop partition
	, wholeDiskLoopDev :: FilePath -- ^ corresponding device for the whole loop disk
	} deriving (Show)

isLoopDev :: LoopDev -> IO Bool
isLoopDev l = isLoopDev' (partitionLoopDev l) <&&> isLoopDev' (wholeDiskLoopDev l)

isLoopDev' :: FilePath -> IO Bool
isLoopDev' f
	| "loop" `isInfixOf` f = catchBoolIO $
		isBlockDevice <$> getFileStatus f
	| otherwise = return False

-- | Uses the kpartx utility to create device maps for partitions contained
-- within a disk image file. The resulting loop devices are passed to the
-- property, which can operate on them. Always cleans up after itself,
-- by removing the device maps after the property is run.
kpartx :: FilePath -> ([LoopDev] -> Property DebianLike) -> Property DebianLike
kpartx diskimage mkprop = go `requires` Apt.installed ["kpartx"]
  where
	go :: Property DebianLike
	go = property' (getDesc (mkprop [])) $ \w -> do
		cleanup -- idempotency
		loopdevs <- liftIO $ kpartxParse
			<$> readProcess "kpartx" ["-avs", diskimage]
		bad <- liftIO $ filterM (not <$$> isLoopDev) loopdevs
		unless (null bad) $
			error $ "kpartx output seems to include non-loop-devices (possible parse failure): " ++ show bad
		r <- ensureProperty w (mkprop loopdevs)
		cleanup
		return r
	cleanup = void $ liftIO $ boolSystem "kpartx" [Param "-d", File diskimage]

kpartxParse :: String -> [LoopDev]
kpartxParse = mapMaybe (finddev . words) . lines
  where
	finddev ("add":"map":ld:_:_:_:_:wd:_) = Just $ LoopDev 
		{ partitionLoopDev = "/dev/mapper/" ++ ld
		, wholeDiskLoopDev = wd
		}
	finddev _ = Nothing
