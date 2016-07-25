module Propellor.Property.Service where

import Propellor.Base

type ServiceName = String

-- | Ensures that a service is running. Does not ensure that
-- any package providing that service is installed. See
-- Apt.serviceInstalledRunning
--
-- Note that due to the general poor state of init scripts, the best
-- we can do is try to start the service, and if it fails, assume
-- this means it's already running.
running :: ServiceName -> Property DebianLike
running = signaled "start" "running"

restarted :: ServiceName -> Property DebianLike
restarted = signaled "restart" "restarted"

reloaded :: ServiceName -> Property DebianLike
reloaded = signaled "reload" "reloaded"

signaled :: String -> Desc -> ServiceName -> Property DebianLike
signaled cmd desc svc = tightenTargets $ p `describe` (desc ++ " " ++ svc)
  where
	p = scriptProperty ["service " ++ shellEscape svc ++ " " ++ cmd ++ " >/dev/null 2>&1 || true"]
		`assume` NoChange
