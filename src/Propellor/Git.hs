module Propellor.Git where

import Utility.Process
import Utility.Exception
import Utility.Directory

import Control.Applicative
import Prelude

getCurrentBranch :: IO String
getCurrentBranch = takeWhile (/= '\n')
	<$> readProcess "git" ["symbolic-ref", "--short", "HEAD"]

getCurrentBranchRef :: IO String
getCurrentBranchRef = takeWhile (/= '\n')
	<$> readProcess "git" ["symbolic-ref", "HEAD"]

getCurrentGitSha1 :: String -> IO String
getCurrentGitSha1 branchref = takeWhile (/= '\n')
	<$> readProcess "git" ["show-ref", "--hash", branchref]

hasOrigin :: IO Bool
hasOrigin = catchDefaultIO False $ do
	rs <- lines <$> readProcess "git" ["remote"]
	return $ "origin" `elem` rs

hasGitRepo :: IO Bool
hasGitRepo = doesFileExist ".git/HEAD"
