-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Support for the Borg backup tool <https://github.com/borgbackup>

module Propellor.Property.Borg
	( installed
	, repoExists
	, init
	, restored
	, backup
	, KeepPolicy (..)
	) where

import Propellor.Base hiding (init)
import Prelude hiding (init)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Cron as Cron
import Data.List (intercalate)

type BorgParam = String

type BorgRepo = FilePath

installed :: Property DebianLike
installed = withOS desc $ \w o -> case o of
	(Just (System (Debian _ (Stable "jessie")) _)) -> ensureProperty w $
		Apt.installedBackport ["borgbackup"]
	_ -> ensureProperty w $
		Apt.installed ["borgbackup"]
  where
        desc = "installed borgbackup"

repoExists :: BorgRepo -> IO Bool
repoExists repo = boolSystem "borg" [Param "list", File repo]

-- | Inits a new borg repository
init :: BorgRepo -> Property DebianLike
init backupdir = check (not <$> repoExists backupdir) (cmdProperty "borg" initargs)
	`requires` installed
  where
	initargs =
		[ "init"
		, backupdir
		]

-- | Restores a directory from an borg backup.
--
-- Only does anything if the directory does not exist, or exists,
-- but is completely empty.
--
-- The restore is performed atomically; restoring to a temp directory
-- and then moving it to the directory.
restored :: FilePath -> BorgRepo -> Property DebianLike
restored dir backupdir = go `requires` installed
  where
	go :: Property DebianLike
	go = property (dir ++ " restored by borg") $ ifM (liftIO needsRestore)
		( do
			warningMessage $ dir ++ " is empty/missing; restoring from backup ..."
			liftIO restore
		, noChange
		)

	needsRestore = null <$> catchDefaultIO [] (dirContents dir)

	restore = withTmpDirIn (takeDirectory dir) "borg-restore" $ \tmpdir -> do
		ok <- boolSystem "borg" $
			[ Param "extract"
			, Param backupdir
			, Param tmpdir
			]
		let restoreddir = tmpdir ++ "/" ++ dir
		ifM (pure ok <&&> doesDirectoryExist restoreddir)
			( do
				void $ tryIO $ removeDirectory dir
				renameDirectory restoreddir dir
				return MadeChange
			, return FailedChange
			)

-- | Installs a cron job that causes a given directory to be backed
-- up, by running borg with some parameters.
--
-- If the directory does not exist, or exists but is completely empty,
-- this Property will immediately restore it from an existing backup.
--
-- So, this property can be used to deploy a directory of content
-- to a host, while also ensuring any changes made to it get backed up.
-- For example:
--
-- >	& Borg.backup "/srv/git" "root@myserver:/mnt/backup/git.borg" Cron.Daily
-- >		["--exclude=/srv/git/tobeignored"]
-- >		[Borg.KeepDays 7, Borg.KeepWeeks 4, Borg.KeepMonths 6, Borg.KeepYears 1]
--
-- Note that this property does not make borg encrypt the backup
-- repository.
--
-- Since borg uses a fair amount of system resources, only one borg
-- backup job will be run at a time. Other jobs will wait their turns to
-- run.
backup :: FilePath -> BorgRepo -> Cron.Times -> [BorgParam] -> [KeepPolicy] -> Property DebianLike
backup dir backupdir crontimes extraargs kp = backup' dir backupdir crontimes extraargs kp
	`requires` restored dir backupdir

-- | Does a backup, but does not automatically restore.
backup' :: FilePath -> BorgRepo -> Cron.Times -> [BorgParam] -> [KeepPolicy] -> Property DebianLike
backup' dir backupdir crontimes extraargs kp = cronjob
	`describe` desc
	`requires` installed
  where
	desc = backupdir ++ " borg backup"
	cronjob = Cron.niceJob ("borg_backup" ++ dir) crontimes (User "root") "/" $
		"flock " ++ shellEscape lockfile ++ " sh -c " ++ backupcmd
	lockfile = "/var/lock/propellor-borg.lock"
	backupcmd = intercalate ";" $
		createCommand
		: if null kp then [] else [pruneCommand]
	createCommand = unwords $
		[ "borg"
		, "create"
		, "--stats"
		]
		++ map shellEscape extraargs ++
		[ shellEscape backupdir ++ "::" ++ "$(date --iso-8601=ns --utc)"
		, shellEscape dir
		]
	pruneCommand = unwords $
		[ "borg"
		, "prune"
		, shellEscape backupdir
		]
		++
		map keepParam kp

-- | Constructs an BorgParam that specifies which old backup generations to
-- keep. By default, all generations are kept. However, when this parameter is
-- passed to the `backup` property, they will run borg prune to clean out
-- generations not specified here.
keepParam :: KeepPolicy -> BorgParam
keepParam (KeepHours n) = "--keep-hourly=" ++ show n
keepParam (KeepDays n) = "--keep-daily=" ++ show n
keepParam (KeepWeeks n) = "--keep-daily=" ++ show n
keepParam (KeepMonths n) = "--keep-monthly=" ++ show n
keepParam (KeepYears n) = "--keep-yearly=" ++ show n

-- | Policy for backup generations to keep. For example, KeepDays 30 will
-- keep the latest backup for each day when a backup was made, and keep the
-- last 30 such backups. When multiple KeepPolicies are combined together,
-- backups meeting any policy are kept. See borg's man page for details.
data KeepPolicy
	= KeepHours Int
	| KeepDays Int
	| KeepWeeks Int
	| KeepMonths Int
	| KeepYears Int
