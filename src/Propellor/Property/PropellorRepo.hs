module Propellor.Property.PropellorRepo where

import Propellor.Base
import Propellor.Git.Config

-- | Sets the url to use as the origin of propellor's git repository.
--
-- When propellor --spin is used to update a host, the url is taken from
-- the repository that --spin is run in, and passed to the host. So, you
-- don't need to specifiy this property then. 
--
-- This property is useful when hosts are being updated without using
-- --spin, eg when using the `Propellor.Property.Cron.runPropellor` cron job.
hasOriginUrl :: String -> Property UnixLike
hasOriginUrl u = property ("propellor repo url " ++ u) $ do
	curru <- liftIO getRepoUrl
	if curru == Just u
		then return NoChange
		else makeChange $ setRepoUrl u
