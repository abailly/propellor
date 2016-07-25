module Propellor.Property.OpenId where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Apache as Apache

import Data.List

-- | Openid provider, using the simpleid PHP CGI, with apache.
--
-- Runs on usual port by default. When a nonstandard port is specified,
-- apache is limited to listening only on that port. Warning: Specifying
-- a port won't compose well with other apache properties on the same
-- host.
--
-- It's probably a good idea to put this property inside a docker or
-- systemd-nspawn container.
providerFor :: [User] -> HostName -> Maybe Port -> Property (HasInfo + DebianLike)
providerFor users hn mp = propertyList desc $ props
	& Apt.serviceInstalledRunning "apache2"
	& apacheconfigured
	& Apt.installed ["simpleid"]
		`onChange` Apache.restarted
	& File.fileProperty (desc ++ " configured")
		(map setbaseurl) "/etc/simpleid/config.inc"
	& propertyList desc (toProps $ map identfile users)
  where
	baseurl = hn ++ case mp of
		Nothing -> ""
		Just p -> ':' : fromPort p
	url = "http://"++baseurl++"/simpleid"
	desc = "openid provider " ++ url
	setbaseurl l
		| "SIMPLEID_BASE_URL" `isInfixOf` l =
			"define('SIMPLEID_BASE_URL', '"++url++"');"
		| otherwise = l

	apacheconfigured = case mp of
		Nothing -> setupRevertableProperty $
			Apache.virtualHost hn (Port 80) "/var/www/html"
		Just p -> propertyList desc $ props
			& Apache.listenPorts [p]
			& Apache.virtualHost hn p "/var/www/html"

	-- the identities directory controls access, so open up
	-- file mode
	identfile (User u) = File.hasPrivContentExposed
		(concat [ "/var/lib/simpleid/identities/", u, ".identity" ])
		(Context baseurl)
