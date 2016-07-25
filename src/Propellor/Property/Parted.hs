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
import qualified Propellor.Property.Partition as Partition
import Utility.DataUnits
import Data.Char
import System.Posix.Files

class PartedVal a where
	val :: a -> String

-- | Types of partition tables supported by parted.
data TableType = MSDOS | GPT | AIX | AMIGA | BSD | DVH | LOOP | MAC | PC98 | SUN
	deriving (Show)

instance PartedVal TableType where
	val = map toLower . show

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
	val Primary = "primary"
	val Logical = "logical"
	val Extended = "extended"

-- | All partition sizing is done in megabytes, so that parted can
-- automatically lay out the partitions.
--
-- Note that these are SI megabytes, not mebibytes.
newtype PartSize = MegaBytes Integer
	deriving (Show)

instance PartedVal PartSize where
	val (MegaBytes n)
		| n > 0 = show n ++ "MB"
		-- parted can't make partitions smaller than 1MB;
		-- avoid failure in edge cases
		| otherwise = show "1MB"

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
	val BootFlag = "boot"
	val RootFlag = "root"
	val SwapFlag = "swap"
	val HiddenFlag = "hidden"
	val RaidFlag = "raid"
	val LvmFlag = "lvm"
	val LbaFlag = "lba"
	val LegacyBootFlag = "legacy_boot"
	val IrstFlag = "irst"
	val EspFlag = "esp"
	val PaloFlag = "palo"

instance PartedVal Bool where
	val True = "on"
	val False = "off"

instance PartedVal Partition.Fs where
	val Partition.EXT2 = "ext2"
	val Partition.EXT3 = "ext3"
	val Partition.EXT4 = "ext4"
	val Partition.BTRFS = "btrfs"
	val Partition.REISERFS = "reiserfs"
	val Partition.XFS = "xfs"
	val Partition.FAT = "fat"
	val Partition.VFAT = "vfat"
	val Partition.NTFS = "ntfs"
	val Partition.LinuxSwap = "linux-swap"

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
	mklabel = ["mklabel", val tabletype]
	mkflag partnum (f, b) =
		[ "set"
		, show partnum
		, val f
		, val b
		]
	mkpart partnum offset p =
		[ "mkpart"
		, val (partType p)
		, val (partFs p)
		, val offset
		, val (offset <> partSize p)
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
parted :: Eep -> FilePath -> [String] -> Property DebianLike
parted YesReallyDeleteDiskContents disk ps = p `requires` installed
  where
	p = cmdProperty "parted" ("--script":"--align":"cylinder":disk:ps)
		`assume` MadeChange

-- | Gets parted installed.
installed :: Property DebianLike
installed = Apt.installed ["parted"]
