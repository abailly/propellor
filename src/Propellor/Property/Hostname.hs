module Propellor.Property.Hostname where

import Propellor.Base
import qualified Propellor.Property.File as File
import Propellor.Property.Chroot (inChroot)

import Data.List
import Data.List.Utils

-- | Ensures that the hostname is set using best practices, to whatever
-- name the `Host` has.
--
-- Configures both </etc/hostname> and the current hostname.
-- (However, when used inside a chroot, avoids setting the current hostname
-- as that would impact the system outside the chroot.)
--
-- Configures </etc/mailname> with the domain part of the hostname.
--
-- </etc/hosts> is also configured, with an entry for 127.0.1.1, which is
-- standard at least on Debian to set the FDQN.
--
-- Also, the </etc/hosts> 127.0.0.1 line is set to localhost. Putting any
-- other hostnames there is not best practices and can lead to annoying
-- messages from eg, apache.
sane :: Property UnixLike
sane = sane' extractDomain

sane' :: ExtractDomain -> Property UnixLike
sane' extractdomain = property' ("sane hostname") $ \w ->
	ensureProperty w . setTo' extractdomain =<< asks hostName

-- Like `sane`, but you can specify the hostname to use, instead
-- of the default hostname of the `Host`.
setTo :: HostName -> Property UnixLike
setTo = setTo' extractDomain

setTo' :: ExtractDomain -> HostName -> Property UnixLike
setTo' extractdomain hn = combineProperties desc $ toProps
	[ "/etc/hostname" `File.hasContent` [basehost]
	, hostslines $ catMaybes
		[ if null domain
			then Nothing 
			else Just ("127.0.1.1", [hn, basehost])
		, Just ("127.0.0.1", ["localhost"])
		]
	, check (not <$> inChroot) $
		cmdProperty "hostname" [basehost]
			`assume` NoChange
	, "/etc/mailname" `File.hasContent`
		[if null domain then hn else domain]
	]
  where
	desc = "hostname " ++ hn
	basehost = takeWhile (/= '.') hn
	domain = extractdomain hn
	
	hostslines ipsnames = 
		File.fileProperty desc (addhostslines ipsnames) "/etc/hosts"
	addhostslines :: [(String, [String])] -> [String] -> [String]
	addhostslines ipsnames ls =
		let ips = map fst ipsnames
		    hasip l = maybe False (`elem` ips) (headMaybe (words l))
		    mkline (ip, names) = ip ++ "\t" ++ (unwords names)
		in map mkline ipsnames ++ filter (not . hasip) ls

-- | Makes </etc/resolv.conf> contain search and domain lines for 
-- the domain that the hostname is in.
searchDomain :: Property UnixLike
searchDomain = searchDomain' extractDomain

searchDomain' :: ExtractDomain -> Property UnixLike
searchDomain' extractdomain = property' desc $ \w ->
	(ensureProperty w . go =<< asks hostName)
  where
	desc = "resolv.conf search and domain configured"
	go hn =
		let domain = extractdomain hn
		in  File.fileProperty desc (use domain) "/etc/resolv.conf"
	use domain ls = filter wanted $ nub (ls ++ cfgs)
	  where
		cfgs = ["domain " ++ domain, "search " ++ domain]
		wanted l
			| l `elem` cfgs = True
			| "domain " `isPrefixOf` l = False
			| "search " `isPrefixOf` l = False
			| otherwise = True

-- | Function to extract the domain name from a HostName.
type ExtractDomain = HostName -> String

-- | hostname of foo.example.com has a domain of example.com.
-- But, when the hostname is example.com, the domain is
-- example.com too.
--
-- This doesn't work for eg, foo.co.uk, or when foo.sci.uni.edu
-- is in a sci.uni.edu subdomain. If you are in such a network,
-- provide your own ExtractDomain function to the properties above.
extractDomain :: ExtractDomain
extractDomain hn = 
	let bits = split "." hn
	in intercalate "." $
		if length bits > 2
			then drop 1 bits
			else bits
