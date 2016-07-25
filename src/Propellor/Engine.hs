{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DataKinds #-}

module Propellor.Engine (
	mainProperties,
	runPropellor,
	ensureChildProperties,
	fromHost,
	fromHost',
	onlyProcess,
) where

import System.Exit
import System.IO
import Data.Monoid
import "mtl" Control.Monad.RWS.Strict
import System.PosixCompat
import System.Posix.IO
import System.FilePath
import Control.Applicative
import Prelude

import Propellor.Types
import Propellor.Types.MetaTypes
import Propellor.Types.Core
import Propellor.Message
import Propellor.Exception
import Propellor.Info
import Utility.Exception
import Utility.Directory

-- | Gets the Properties of a Host, and ensures them all,
-- with nice display of what's being done.
mainProperties :: Host -> IO ()
mainProperties host = do
	ret <- runPropellor host $ ensureChildProperties [toChildProperty overall]
	messagesDone
	case ret of
		FailedChange -> exitWith (ExitFailure 1)
		_ -> exitWith ExitSuccess
  where
	overall :: Property (MetaTypes '[])
	overall = property "overall" $
		ensureChildProperties (hostProperties host)

-- | Runs a Propellor action with the specified host.
--
-- If the Result is not FailedChange, any EndActions
-- that were accumulated while running the action
-- are then also run.
runPropellor :: Host -> Propellor Result -> IO Result
runPropellor host a = do
	(res, endactions) <- evalRWST (runWithHost a) host ()
	endres <- mapM (runEndAction host res) endactions
	return $ mconcat (res:endres)

runEndAction :: Host -> Result -> EndAction -> IO Result
runEndAction host res (EndAction desc a) = actionMessageOn (hostName host) desc $ do
	(ret, _s, _) <- runRWST (runWithHost (catchPropellor (a res))) host ()
	return ret

-- | Ensures the child properties, with a display of each as it runs.
ensureChildProperties :: [ChildProperty] -> Propellor Result
ensureChildProperties ps = ensure ps NoChange
  where
	ensure [] rs = return rs
	ensure (p:ls) rs = do
		hn <- asks hostName
		r <- actionMessageOn hn (getDesc p) (catchPropellor $ getSatisfy p)
		ensure ls (r <> rs)

-- | Lifts an action into the context of a different host.
--
-- > fromHost hosts "otherhost" Ssh.getHostPubKey
fromHost :: [Host] -> HostName -> Propellor a -> Propellor (Maybe a)
fromHost l hn getter = case findHost l hn of
	Nothing -> return Nothing
	Just h -> Just <$> fromHost' h getter

fromHost' :: Host -> Propellor a -> Propellor a
fromHost' h getter = do
	(ret, _s, runlog) <- liftIO $ runRWST (runWithHost getter) h ()
	tell runlog
	return ret

onlyProcess :: FilePath -> IO a -> IO a
onlyProcess lockfile a = bracket lock unlock (const a)
  where
	lock = do
		createDirectoryIfMissing True (takeDirectory lockfile)
		l <- createFile lockfile stdFileMode
		setLock l (WriteLock, AbsoluteSeek, 0, 0)
			`catchIO` const alreadyrunning
		return l
	unlock = closeFd
	alreadyrunning = error "Propellor is already running on this host!"
