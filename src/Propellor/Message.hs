-- | This module handles all display of output to the console when
-- propellor is ensuring Properties.
--
-- When two threads both try to display a message concurrently, 
-- the messages will be displayed sequentially.

module Propellor.Message (
	getMessageHandle,
	isConsole,
	forceConsole,
	actionMessage,
	actionMessageOn,
	warningMessage,
	infoMessage,
	errorMessage,
	stopPropellorMessage,
	processChainOutput,
	messagesDone,
	createProcessConcurrent,
	withConcurrentOutput,
) where

import System.Console.ANSI
import System.IO
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.IO.Unsafe (unsafePerformIO)
import Control.Concurrent
import System.Console.Concurrent
import Control.Applicative
import Prelude

import Propellor.Types
import Propellor.Types.Exception
import Utility.PartialPrelude
import Utility.Monad
import Utility.Exception

data MessageHandle = MessageHandle
	{ isConsole :: Bool
	}

-- | A shared global variable for the MessageHandle.
{-# NOINLINE globalMessageHandle #-}
globalMessageHandle :: MVar MessageHandle
globalMessageHandle = unsafePerformIO $ 
	newMVar =<< MessageHandle
		<$> catchDefaultIO False (hIsTerminalDevice stdout)

-- | Gets the global MessageHandle.
getMessageHandle :: IO MessageHandle
getMessageHandle = readMVar globalMessageHandle

-- | Force console output. This can be used when stdout is not directly
-- connected to a console, but is eventually going to be displayed at a
-- console.
forceConsole :: IO ()
forceConsole = modifyMVar_ globalMessageHandle $ \mh ->
	pure (mh { isConsole = True })

whenConsole :: String -> IO String
whenConsole s = ifM (isConsole <$> getMessageHandle)
	( pure s
	, pure ""
	)

-- | Shows a message while performing an action, with a colored status
-- display.
actionMessage :: (MonadIO m, MonadMask m, ActionResult r) => Desc -> m r -> m r
actionMessage = actionMessage' Nothing

-- | Shows a message while performing an action on a specified host,
-- with a colored status display.
actionMessageOn :: (MonadIO m, MonadMask m, ActionResult r) => HostName -> Desc -> m r -> m r
actionMessageOn = actionMessage' . Just

actionMessage' :: (MonadIO m, MonadMask m, ActionResult r) => Maybe HostName -> Desc -> m r -> m r
actionMessage' mhn desc a = do
	liftIO $ outputConcurrent
		=<< whenConsole (setTitleCode $ "propellor: " ++ desc)

	r <- a

	liftIO $ outputConcurrent . concat =<< sequence
		[ whenConsole $
			setTitleCode "propellor: running"
		, showhn mhn
		, pure $ desc ++ " ... "
		, let (msg, intensity, color) = getActionResult r
		  in colorLine intensity color msg
		]

	return r
  where
	showhn Nothing = return ""
	showhn (Just hn) = concat <$> sequence
		[ whenConsole $
			setSGRCode [SetColor Foreground Dull Cyan]
		, pure (hn ++ " ")
		, whenConsole $
			setSGRCode []
		]

warningMessage :: MonadIO m => String -> m ()
warningMessage s = liftIO $
	outputConcurrent =<< colorLine Vivid Magenta ("** warning: " ++ s)

infoMessage :: MonadIO m => [String] -> m ()
infoMessage ls = liftIO $ outputConcurrent $ concatMap (++ "\n") ls

-- | Displays the error message in red, and throws an exception.
--
-- When used inside a property, the exception will make the current
-- property fail. Propellor will continue to the next property.
errorMessage :: MonadIO m => String -> m a
errorMessage s = liftIO $ do
	outputConcurrent =<< colorLine Vivid Red ("** error: " ++ s)
	-- Normally this exception gets caught and is not displayed,
	-- and propellor continues. So it's only displayed if not
	-- caught, and so we say, cannot continue.
	error "Cannot continue!"
 
-- | Like `errorMessage`, but throws a `StopPropellorException`,
-- preventing propellor from continuing to the next property.
--
-- Think twice before using this. Is the problem so bad that propellor
-- cannot try to ensure other properties? If not, use `errorMessage`
-- instead.
stopPropellorMessage :: MonadIO m => String -> m a
stopPropellorMessage s = liftIO $ do
	outputConcurrent =<< colorLine Vivid Red ("** fatal error: " ++ s)
	throwM $ StopPropellorException "Cannot continue!"

colorLine :: ColorIntensity -> Color -> String -> IO String
colorLine intensity color msg = concat <$> sequence
	[ whenConsole $
		setSGRCode [SetColor Foreground intensity color]
	, pure msg
	, whenConsole $
		setSGRCode []
	-- Note this comes after the color is reset, so that
	-- the color set and reset happen in the same line.
	, pure "\n"
	]

-- | Reads and displays each line from the Handle, except for the last line
-- which is a Result.
processChainOutput :: Handle -> IO Result
processChainOutput h = go Nothing
  where
	go lastline = do
		v <- catchMaybeIO (hGetLine h)
		case v of
			Nothing -> case lastline of
				Nothing -> do
					return FailedChange
				Just l -> case readish l of
					Just r -> pure r
					Nothing -> do
						outputConcurrent (l ++ "\n")
						return FailedChange
			Just s -> do
				outputConcurrent $
					maybe "" (\l -> if null l then "" else l ++ "\n") lastline
				go (Just s)

-- | Called when all messages about properties have been printed.
messagesDone :: IO ()
messagesDone = outputConcurrent
	=<< whenConsole (setTitleCode "propellor: done")
