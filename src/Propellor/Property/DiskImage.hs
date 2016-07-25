-- | Disk image generation.
--
-- This module is designed to be imported unqualified.

{-# LANGUAGE TypeFamilies #-}

module Propellor.Property.DiskImage (
	-- * Partition specification
	module Propellor.Property.DiskImage.PartSpec,
	-- * Properties
	DiskImage,
	imageBuilt,
	imageRebuilt,
	imageBuiltFrom,
	imageExists,
	-- * Finalization
	Finalization,
	grubBooted,
	Grub.BIOS(..),
	noFinalization,
) where

import Propellor.Base
import Propellor.Property.DiskImage.PartSpec
import Propellor.Property.Chroot (Chroot)
import Propellor.Property.Chroot.Util (removeChroot)
import qualified Propellor.Property.Chroot as Chroot
import qualified Propellor.Property.Grub as Grub
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import Propellor.Property.Parted
import Propellor.Property.Mount
import Propellor.Property.Fstab (SwapPartition(..), genFstab)
import Propellor.Property.Partition
import Propellor.Property.Rsync
import Propellor.Container
import Utility.Path

import Data.List (isPrefixOf, isInfixOf, sortBy)
import Data.Function (on)
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Lazy as L
import System.Posix.Files

type DiskImage = FilePath

-- | Creates a bootable disk image.
--
-- First the specified Chroot is set up, and its properties are satisfied.
--
-- Then, the disk image is set up, and the chroot is copied into the
-- appropriate partition(s) of it.
--
-- Example use:
--
-- > import Propellor.Property.DiskImage
--
-- > let chroot d = Chroot.debootstrapped mempty d
-- >	& osDebian Unstable X86_64
-- >	& Apt.installed ["linux-image-amd64"]
-- >	& User.hasPassword (User "root")
-- >	& User.accountFor (User "demo")
-- > 	& User.hasPassword (User "demo")
-- >	& User.hasDesktopGroups (User "demo")
-- > 	& ...
-- > in imageBuilt "/srv/images/foo.img" chroot
-- >	MSDOS (grubBooted PC)
-- >	[ partition EXT2 `mountedAt` "/boot"
-- >		`setFlag` BootFlag
-- >	, partition EXT4 `mountedAt` "/"
-- >		`addFreeSpace` MegaBytes 100
-- >		`mountOpt` errorReadonly
-- >	, swapPartition (MegaBytes 256)
-- >	]
--
-- Note that the disk image file is reused if it already exists,
-- to avoid expensive IO to generate a new one. And, it's updated in-place,
-- so its contents are undefined during the build process.
--
-- Note that the `Chroot.noServices` property is automatically added to the
-- chroot while the disk image is being built, which should prevent any
-- daemons that are included from being started on the system that is
-- building the disk image.
imageBuilt :: DiskImage -> (FilePath -> Chroot) -> TableType -> Finalization -> [PartSpec] -> RevertableProperty (HasInfo + Linux) Linux
imageBuilt = imageBuilt' False

-- | Like 'built', but the chroot is deleted and rebuilt from scratch each
-- time. This is more expensive, but useful to ensure reproducible results
-- when the properties of the chroot have been changed.
imageRebuilt :: DiskImage -> (FilePath -> Chroot) -> TableType -> Finalization -> [PartSpec] -> RevertableProperty (HasInfo + Linux) Linux
imageRebuilt = imageBuilt' True

imageBuilt' :: Bool -> DiskImage -> (FilePath -> Chroot) -> TableType -> Finalization -> [PartSpec] -> RevertableProperty (HasInfo + Linux) Linux
imageBuilt' rebuild img mkchroot tabletype final partspec =
	imageBuiltFrom img chrootdir tabletype final partspec
		`requires` Chroot.provisioned chroot
		`requires` (cleanrebuild <!> (doNothing :: Property UnixLike))
		`describe` desc
  where
	desc = "built disk image " ++ img
	cleanrebuild :: Property Linux
	cleanrebuild
		| rebuild = property desc $ do
			liftIO $ removeChroot chrootdir
			return MadeChange
		| otherwise = doNothing
	chrootdir = img ++ ".chroot"
	chroot =
		let c = mkchroot chrootdir
		in setContainerProps c $ containerProps c
			-- Before ensuring any other properties of the chroot,
			-- avoid starting services. Reverted by imageFinalized.
			&^ Chroot.noServices
			-- First stage finalization.
			& fst final
			& cachesCleaned

-- | This property is automatically added to the chroot when building a
-- disk image. It cleans any caches of information that can be omitted;
-- eg the apt cache on Debian.
cachesCleaned :: Property UnixLike
cachesCleaned = "cache cleaned" ==> (Apt.cacheCleaned `pickOS` skipit)
  where
	skipit = doNothing :: Property UnixLike

-- | Builds a disk image from the contents of a chroot.
imageBuiltFrom :: DiskImage -> FilePath -> TableType -> Finalization -> [PartSpec] -> RevertableProperty (HasInfo + Linux) UnixLike
imageBuiltFrom img chrootdir tabletype final partspec = mkimg <!> rmimg
  where
	desc = img ++ " built from " ++ chrootdir
	mkimg = property' desc $ \w -> do
		-- unmount helper filesystems such as proc from the chroot
		-- before getting sizes
		liftIO $ unmountBelow chrootdir
		szm <- M.mapKeys (toSysDir chrootdir) . M.map toPartSize
			<$> liftIO (dirSizes chrootdir)
		let calcsz mnts = maybe defSz fudge . getMountSz szm mnts
		-- tie the knot!
		let (mnts, mntopts, parttable) = fitChrootSize tabletype partspec $
			map (calcsz mnts) mnts
		ensureProperty w $
			imageExists img (partTableSize parttable)
				`before`
			partitioned YesReallyDeleteDiskContents img parttable
				`before`
			kpartx img (mkimg' mnts mntopts parttable)
	mkimg' mnts mntopts parttable devs =
		partitionsPopulated chrootdir mnts mntopts devs
			`before`
		imageFinalized final mnts mntopts devs parttable
	rmimg = File.notPresent img

partitionsPopulated :: FilePath -> [Maybe MountPoint] -> [MountOpts] -> [LoopDev] -> Property Linux
partitionsPopulated chrootdir mnts mntopts devs = property' desc $ \w ->
	mconcat $ zipWith3 (go w) mnts mntopts devs
  where
	desc = "partitions populated from " ++ chrootdir

	go _ Nothing _ _ = noChange
	go w (Just mnt) mntopt loopdev = withTmpDir "mnt" $ \tmpdir -> bracket
		(liftIO $ mount "auto" (partitionLoopDev loopdev) tmpdir mntopt)
		(const $ liftIO $ umountLazy tmpdir)
		$ \ismounted -> if ismounted
			then ensureProperty w $
				syncDirFiltered (filtersfor mnt) (chrootdir ++ mnt) tmpdir
			else return FailedChange

	filtersfor mnt =
		let childmnts = map (drop (length (dropTrailingPathSeparator mnt))) $
			filter (\m -> m /= mnt && addTrailingPathSeparator mnt `isPrefixOf` m)
				(catMaybes mnts)
		in concatMap (\m ->
			-- Include the child mount point, but exclude its contents.
			[ Include (Pattern m)
			, Exclude (filesUnder m)
			-- Preserve any lost+found directory that mkfs made
			, Protect (Pattern "lost+found")
			]) childmnts

-- The constructor for each Partition is passed the size of the files
-- from the chroot that will be put in that partition.
fitChrootSize :: TableType -> [PartSpec] -> [PartSize] -> ([Maybe MountPoint], [MountOpts], PartTable)
fitChrootSize tt l basesizes = (mounts, mountopts, parttable)
  where
	(mounts, mountopts, sizers) = unzip3 l
	parttable = PartTable tt (zipWith id sizers basesizes)

-- | Generates a map of the sizes of the contents of
-- every directory in a filesystem tree.
--
-- (Hard links are counted multiple times for simplicity)
--
-- Should be same values as du -bl
dirSizes :: FilePath -> IO (M.Map FilePath Integer)
dirSizes top = go M.empty top [top]
  where
	go m _ [] = return m
	go m dir (i:is) = flip catchIO (\_ioerr -> go m dir is) $ do
		s <- getSymbolicLinkStatus i
		let sz = fromIntegral (fileSize s)
		if isDirectory s
			then do
				subm <- go M.empty i =<< dirContents i
				let sz' = M.foldr' (+) sz
					(M.filterWithKey (const . subdirof i) subm)
				go (M.insertWith (+) i sz' (M.union m subm)) dir is
			else go (M.insertWith (+) dir sz m) dir is
	subdirof parent i = not (i `equalFilePath` parent) && takeDirectory i `equalFilePath` parent

getMountSz :: (M.Map FilePath PartSize) -> [Maybe MountPoint] -> Maybe MountPoint -> Maybe PartSize
getMountSz _ _ Nothing = Nothing
getMountSz szm l (Just mntpt) =
	fmap (`reducePartSize` childsz) (M.lookup mntpt szm)
  where
	childsz = mconcat $ mapMaybe (getMountSz szm l) (filter (isChild mntpt) l)

-- | Ensures that a disk image file of the specified size exists.
--
-- If the file doesn't exist, or is too small, creates a new one, full of 0's.
--
-- If the file is too large, truncates it down to the specified size.
imageExists :: FilePath -> ByteSize -> Property Linux
imageExists img sz = property ("disk image exists" ++ img) $ liftIO $ do
	ms <- catchMaybeIO $ getFileStatus img
	case ms of
		Just s
			| toInteger (fileSize s) == toInteger sz -> return NoChange
			| toInteger (fileSize s) > toInteger sz -> do
				setFileSize img (fromInteger sz)
				return MadeChange
		_ -> do
			L.writeFile img (L.replicate (fromIntegral sz) 0)
			return MadeChange

-- | A pair of properties. The first property is satisfied within the
-- chroot, and is typically used to download the boot loader.
--
-- The second property is run after the disk image is created,
-- with its populated partition tree mounted in the provided
-- location from the provided loop devices. This will typically
-- take care of installing the boot loader to the image.
--
-- It's ok if the second property leaves additional things mounted
-- in the partition tree.
type Finalization = (Property Linux, (FilePath -> [LoopDev] -> Property Linux))

imageFinalized :: Finalization -> [Maybe MountPoint] -> [MountOpts] -> [LoopDev] -> PartTable -> Property Linux
imageFinalized (_, final) mnts mntopts devs (PartTable _ parts) =
	property' "disk image finalized" $ \w ->
		withTmpDir "mnt" $ \top ->
			go w top `finally` liftIO (unmountall top)
  where
	go w top = do
		liftIO $ mountall top
		liftIO $ writefstab top
		liftIO $ allowservices top
		ensureProperty w $ final top devs

	-- Ordered lexographically by mount point, so / comes before /usr
	-- comes before /usr/local
	orderedmntsdevs :: [(Maybe MountPoint, (MountOpts, LoopDev))]
	orderedmntsdevs = sortBy (compare `on` fst) $ zip mnts (zip mntopts devs)

	swaps = map (SwapPartition . partitionLoopDev . snd) $
		filter ((== LinuxSwap) . partFs . fst) $
			zip parts devs

	mountall top = forM_ orderedmntsdevs $ \(mp, (mopts, loopdev)) -> case mp of
		Nothing -> noop
		Just p -> do
			let mnt = top ++ p
			createDirectoryIfMissing True mnt
			unlessM (mount "auto" (partitionLoopDev loopdev) mnt mopts) $
				error $ "failed mounting " ++ mnt

	unmountall top = do
		unmountBelow top
		umountLazy top

	writefstab top = do
		let fstab = top ++ "/etc/fstab"
		old <- catchDefaultIO [] $ filter (not . unconfigured) . lines
			<$> readFileStrict fstab
		new <- genFstab (map (top ++) (catMaybes mnts))
			swaps (toSysDir top)
		writeFile fstab $ unlines $ new ++ old
	-- Eg "UNCONFIGURED FSTAB FOR BASE SYSTEM"
	unconfigured s = "UNCONFIGURED" `isInfixOf` s

	allowservices top = nukeFile (top ++ "/usr/sbin/policy-rc.d")

noFinalization :: Finalization
noFinalization = (doNothing, \_ _ -> doNothing)

-- | Makes grub be the boot loader of the disk image.
grubBooted :: Grub.BIOS -> Finalization
grubBooted bios = (Grub.installed' bios, boots)
  where
	boots mnt loopdevs = combineProperties "disk image boots using grub" $ props
		-- bind mount host /dev so grub can access the loop devices
		& bindMount "/dev" (inmnt "/dev")
		& mounted "proc" "proc" (inmnt "/proc") mempty
		& mounted "sysfs" "sys" (inmnt "/sys") mempty
		-- update the initramfs so it gets the uuid of the root partition
		& inchroot "update-initramfs" ["-u"]
			`assume` MadeChange
		-- work around for http://bugs.debian.org/802717
		& check haveosprober (inchroot "chmod" ["-x", osprober])
		& inchroot "update-grub" []
			`assume` MadeChange
		& check haveosprober (inchroot "chmod" ["+x", osprober])
		& inchroot "grub-install" [wholediskloopdev]
			`assume` MadeChange
		-- sync all buffered changes out to the disk image
		-- may not be necessary, but seemed needed sometimes
		-- when using the disk image right away.
		& cmdProperty "sync" []
			`assume` NoChange
	  where
	  	-- cannot use </> since the filepath is absolute
		inmnt f = mnt ++ f

		inchroot cmd ps = cmdProperty "chroot" ([mnt, cmd] ++ ps)

		haveosprober = doesFileExist (inmnt osprober)
		osprober = "/etc/grub.d/30_os-prober"

		-- It doesn't matter which loopdev we use; all
		-- come from the same disk image, and it's the loop dev
		-- for the whole disk image we seek.
		wholediskloopdev = case loopdevs of
			(l:_) -> wholeDiskLoopDev l
			[] -> error "No loop devs provided!"

isChild :: FilePath -> Maybe MountPoint -> Bool
isChild mntpt (Just d)
	| d `equalFilePath` mntpt = False
	| otherwise = mntpt `dirContains` d
isChild _ Nothing = False

-- | From a location in a chroot (eg, /tmp/chroot/usr) to
-- the corresponding location inside (eg, /usr).
toSysDir :: FilePath -> FilePath -> FilePath
toSysDir chrootdir d = case makeRelative chrootdir d of
		"." -> "/"
		sysdir -> "/" ++ sysdir
