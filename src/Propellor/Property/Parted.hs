{-# LANGUAGE FlexibleContexts #-}

module Propellor.Property.Parted (
	TableType(..),
	PartTable(..),
	partTableSize,
	Partition(..),
	mkPartition,
	Partition.Fs(..),
	PartSize(..),
	ByteSize,
	toPartSize,
	fromPartSize,
	reducePartSize,
	Partition.MkfsOpts,
	PartType(..),
	PartFlag(..),
	Eep(..),
	partitioned,
	parted,
	installed,
) where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Pacman as Pacman
import qualified Propellor.Property.Partition as Partition
import Utility.DataUnits
import Data.Char
import System.Posix.Files

class PartedVal a where
	pval :: a -> String

-- | Types of partition tables supported by parted.
data TableType = MSDOS | GPT | AIX | AMIGA | BSD | DVH | LOOP | MAC | PC98 | SUN
	deriving (Show)

instance PartedVal TableType where
	pval = map toLower . show

-- | A disk's partition table.
data PartTable = PartTable TableType [Partition]
	deriving (Show)

instance Monoid PartTable where
	-- | default TableType is MSDOS
	mempty = PartTable MSDOS []
	-- | uses the TableType of the second parameter
	mappend (PartTable _l1 ps1) (PartTable l2 ps2) = PartTable l2 (ps1 ++ ps2)

-- | Gets the total size of the disk specified by the partition table.
partTableSize :: PartTable -> ByteSize
partTableSize (PartTable _ ps) = fromPartSize $
	-- add 1 megabyte to hold the partition table itself
	mconcat (MegaBytes 1 : map partSize ps)

-- | A partition on the disk.
data Partition = Partition
	{ partType :: PartType
	, partSize :: PartSize
	, partFs :: Partition.Fs
	, partMkFsOpts :: Partition.MkfsOpts
	, partFlags :: [(PartFlag, Bool)] -- ^ flags can be set or unset (parted may set some flags by default)
	, partName :: Maybe String -- ^ optional name for partition (only works for GPT, PC98, MAC)
	}
	deriving (Show)

-- | Makes a Partition with defaults for non-important values.
mkPartition :: Partition.Fs -> PartSize -> Partition
mkPartition fs sz = Partition
	{ partType = Primary
	, partSize = sz
	, partFs = fs
	, partMkFsOpts = []
	, partFlags = []
	, partName = Nothing
	}

-- | Type of a partition.
data PartType = Primary | Logical | Extended
	deriving (Show)

instance PartedVal PartType where
	pval Primary = "primary"
	pval Logical = "logical"
	pval Extended = "extended"

-- | All partition sizing is done in megabytes, so that parted can
-- automatically lay out the partitions.
--
-- Note that these are SI megabytes, not mebibytes.
newtype PartSize = MegaBytes Integer
	deriving (Show)

instance PartedVal PartSize where
	pval (MegaBytes n)
		| n > 0 = val n ++ "MB"
		-- parted can't make partitions smaller than 1MB;
		-- avoid failure in edge cases
		| otherwise = "1MB"

-- | Rounds up to the nearest MegaByte.
toPartSize :: ByteSize -> PartSize
toPartSize b = MegaBytes $ ceiling (fromInteger b / 1000000 :: Double)

fromPartSize :: PartSize -> ByteSize
fromPartSize (MegaBytes b) = b * 1000000

instance Monoid PartSize where
	mempty = MegaBytes 0
	mappend (MegaBytes a) (MegaBytes b) = MegaBytes (a + b)

reducePartSize :: PartSize -> PartSize -> PartSize
reducePartSize (MegaBytes a) (MegaBytes b) = MegaBytes (a - b)

-- | Flags that can be set on a partition.
data PartFlag = BootFlag | RootFlag | SwapFlag | HiddenFlag | RaidFlag | LvmFlag | LbaFlag | LegacyBootFlag | IrstFlag | EspFlag | PaloFlag
	deriving (Show)

instance PartedVal PartFlag where
	pval BootFlag = "boot"
	pval RootFlag = "root"
	pval SwapFlag = "swap"
	pval HiddenFlag = "hidden"
	pval RaidFlag = "raid"
	pval LvmFlag = "lvm"
	pval LbaFlag = "lba"
	pval LegacyBootFlag = "legacy_boot"
	pval IrstFlag = "irst"
	pval EspFlag = "esp"
	pval PaloFlag = "palo"

instance PartedVal Bool where
	pval True = "on"
	pval False = "off"

instance PartedVal Partition.Fs where
	pval Partition.EXT2 = "ext2"
	pval Partition.EXT3 = "ext3"
	pval Partition.EXT4 = "ext4"
	pval Partition.BTRFS = "btrfs"
	pval Partition.REISERFS = "reiserfs"
	pval Partition.XFS = "xfs"
	pval Partition.FAT = "fat"
	pval Partition.VFAT = "vfat"
	pval Partition.NTFS = "ntfs"
	pval Partition.LinuxSwap = "linux-swap"

data Eep = YesReallyDeleteDiskContents

-- | Partitions a disk using parted, and formats the partitions.
--
-- The FilePath can be a block device (eg, \/dev\/sda), or a disk image file.
--
-- This deletes any existing partitions in the disk! Use with EXTREME caution!
partitioned :: Eep -> FilePath -> PartTable -> Property DebianLike
partitioned eep disk (PartTable tabletype parts) = property' desc $ \w -> do
	isdev <- liftIO $ isBlockDevice <$> getFileStatus disk
	ensureProperty w $ combineProperties desc $ props
		& parted eep disk partedparams
		& if isdev
			then formatl (map (\n -> disk ++ show n) [1 :: Int ..])
			else Partition.kpartx disk (formatl . map Partition.partitionLoopDev)
  where
	desc = disk ++ " partitioned"
	formatl devs = combineProperties desc (toProps $ map format (zip parts devs))
	partedparams = concat $ mklabel : mkparts (1 :: Integer) mempty parts []
	format (p, dev) = Partition.formatted' (partMkFsOpts p)
		Partition.YesReallyFormatPartition (partFs p) dev
	mklabel = ["mklabel", pval tabletype]
	mkflag partnum (f, b) =
		[ "set"
		, show partnum
		, pval f
		, pval b
		]
	mkpart partnum offset p =
		[ "mkpart"
		, pval (partType p)
		, pval (partFs p)
		, pval offset
		, pval (offset <> partSize p)
		] ++ case partName p of
			Just n -> ["name", show partnum, n]
			Nothing -> []
	mkparts partnum offset (p:ps) c = 
		mkparts (partnum+1) (offset <> partSize p) ps
			(c ++ mkpart partnum offset p : map (mkflag partnum) (partFlags p))
	mkparts _ _ [] c = c

-- | Runs parted on a disk with the specified parameters.
--
-- Parted is run in script mode, so it will never prompt for input.
-- It is asked to use cylinder alignment for the disk.
parted :: Eep -> FilePath -> [String] -> Property (DebianLike + ArchLinux)
parted YesReallyDeleteDiskContents disk ps = p `requires` installed
  where
	p = cmdProperty "parted" ("--script":"--align":"cylinder":disk:ps)
		`assume` MadeChange

-- | Gets parted installed.
installed :: Property (DebianLike + ArchLinux)
installed = Apt.installed ["parted"] `pickOS` Pacman.installed ["parted"]
