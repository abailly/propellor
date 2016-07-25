-- | Wrapper program for propellor distribution.
--
-- Distributions should install this program into PATH.
-- (Cabal builds it as dist/build/propellor/propellor).
--
-- This is not the propellor main program (that's config.hs).
-- This bootstraps ~/.propellor/config.hs, builds it if
-- it's not already built, and runs it.

module Main where

import Propellor.DotDir
import Propellor.Message
import Propellor.Bootstrap
import Utility.Monad
import Utility.Directory
import Utility.Process
import Utility.Process.NonConcurrent

import System.Environment (getArgs)
import System.Exit
import System.Posix.Directory
import Control.Monad.IfElse

main :: IO ()
main = withConcurrentOutput $ go =<< getArgs
  where
	go ["--init"] = interactiveInit
	go args = ifM (doesDirectoryExist =<< dotPropellor)
		( do
			checkRepoUpToDate
			buildRunConfig args
		, error "Seems that ~/.propellor/ does not exist. To set it up, run: propellor --init"
		)

buildRunConfig :: [String] -> IO ()
buildRunConfig args = do
	changeWorkingDirectory =<< dotPropellor
	unlessM (doesFileExist "propellor") $ do
		buildPropellor Nothing
		putStrLn ""
		putStrLn ""
	(_, _, _, pid) <- createProcessNonConcurrent (proc "./propellor" args) 
	exitWith =<< waitForProcessNonConcurrent pid
