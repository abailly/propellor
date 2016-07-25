{-# LANGUAGE PackageImports, TypeFamilies, DataKinds, PolyKinds #-}

module Propellor.Info (
	osDebian,
	osBuntish,
	osFreeBSD,
	setInfoProperty,
	addInfoProperty,
	pureInfoProperty,
	pureInfoProperty',
	askInfo,
	getOS,
	ipv4,
	ipv6,
	alias,
	addDNS,
	hostMap,
	aliasMap,
	findHost,
	findHostNoAlias,
	getAddresses,
	hostAddresses,
) where

import Propellor.Types
import Propellor.Types.Info
import Propellor.Types.MetaTypes

import "mtl" Control.Monad.Reader
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Control.Applicative
import Prelude

-- | Adds info to a Property.
--
-- The new Property will include HasInfo in its metatypes.
setInfoProperty
	:: (MetaTypes metatypes' ~ (+) HasInfo metatypes, SingI metatypes')
	=> Property metatypes
	-> Info
	-> Property (MetaTypes metatypes')
setInfoProperty (Property _ d a oldi c) newi =
	Property sing d a (oldi <> newi) c

-- | Adds more info to a Property that already HasInfo.
addInfoProperty
	:: (IncludesInfo metatypes ~ 'True)
	=> Property metatypes
	-> Info
	-> Property metatypes
addInfoProperty (Property t d a oldi c) newi =
	Property t d a (oldi <> newi) c

-- | Makes a property that does nothing but set some `Info`.
pureInfoProperty :: (IsInfo v) => Desc -> v -> Property (HasInfo + UnixLike)
pureInfoProperty desc v = pureInfoProperty' desc (toInfo v)

pureInfoProperty' :: Desc -> Info -> Property (HasInfo + UnixLike)
pureInfoProperty' desc i = setInfoProperty p i
  where
	p :: Property UnixLike
	p = property ("has " ++ desc) (return NoChange)

-- | Gets a value from the host's Info.
askInfo :: (IsInfo v) => Propellor v
askInfo = asks (fromInfo . hostInfo)

-- | Specifies that a host's operating system is Debian,
-- and further indicates the suite and architecture.
-- 
-- This provides info for other Properties, so they can act
-- conditionally on the details of the OS.
--
-- It also lets the type checker know that all the properties of the
-- host must support Debian.
--
-- >	& osDebian (Stable "jessie") X86_64
osDebian :: DebianSuite -> Architecture -> Property (HasInfo + Debian)
osDebian = osDebian' Linux

-- Use to specify a different `DebianKernel` than the default `Linux`
--
-- >	& osDebian' KFreeBSD (Stable "jessie") X86_64
osDebian' :: DebianKernel -> DebianSuite -> Architecture -> Property (HasInfo + Debian)
osDebian' kernel suite arch = tightenTargets $ os (System (Debian kernel suite) arch)

-- | Specifies that a host's operating system is a well-known Debian
-- derivative founded by a space tourist.
--
-- (The actual name of this distribution is not used in Propellor per
-- <http://joeyh.name/blog/entry/trademark_nonsense/>)
osBuntish :: Release -> Architecture -> Property (HasInfo + Buntish)
osBuntish release arch = tightenTargets $ os (System (Buntish release) arch)

-- | Specifies that a host's operating system is FreeBSD
-- and further indicates the release and architecture.
osFreeBSD :: FreeBSDRelease -> Architecture -> Property (HasInfo + FreeBSD)
osFreeBSD release arch = tightenTargets $ os (System (FreeBSD release) arch)

os :: System -> Property (HasInfo + UnixLike)
os system = pureInfoProperty ("Operating " ++ show system) (InfoVal system)

--  Gets the operating system of a host, if it has been specified.
getOS :: Propellor (Maybe System)
getOS = fromInfoVal <$> askInfo

-- | Indicate that a host has an A record in the DNS.
--
-- When propellor is used to deploy a DNS server for a domain,
-- the hosts in the domain are found by looking for these
-- and similar properites.
--
-- When propellor --spin is used to deploy a host, it checks
-- if the host's IP Property matches the DNS. If the DNS is missing or
-- out of date, the host will instead be contacted directly by IP address.
ipv4 :: String -> Property (HasInfo + UnixLike)
ipv4 = addDNS . Address . IPv4

-- | Indicate that a host has an AAAA record in the DNS.
ipv6 :: String -> Property (HasInfo + UnixLike)
ipv6 = addDNS . Address . IPv6

-- | Indicates another name for the host in the DNS.
--
-- When the host's ipv4/ipv6 addresses are known, the alias is set up
-- to use their address, rather than using a CNAME. This avoids various
-- problems with CNAMEs, and also means that when multiple hosts have the
-- same alias, a DNS round-robin is automatically set up.
alias :: Domain -> Property (HasInfo + UnixLike)
alias d = pureInfoProperty' ("alias " ++ d) $ mempty
	`addInfo` toAliasesInfo [d]
	-- A CNAME is added here, but the DNS setup code converts it to an
	-- IP address when that makes sense.
	`addInfo` (toDnsInfo $ S.singleton $ CNAME $ AbsDomain d)

addDNS :: Record -> Property (HasInfo + UnixLike)
addDNS r = pureInfoProperty (rdesc r) (toDnsInfo (S.singleton r))
  where
	rdesc (CNAME d) = unwords ["alias", ddesc d]
	rdesc (Address (IPv4 addr)) = unwords ["ipv4", addr]
	rdesc (Address (IPv6 addr)) = unwords ["ipv6", addr]
	rdesc (MX n d) = unwords ["MX", show n, ddesc d]
	rdesc (NS d) = unwords ["NS", ddesc d]
	rdesc (TXT s) = unwords ["TXT", s]
	rdesc (SRV x y z d) = unwords ["SRV", show x, show y, show z, ddesc d]
	rdesc (SSHFP x y s) = unwords ["SSHFP", show x, show y, s]
	rdesc (INCLUDE f) = unwords ["$INCLUDE", f]
	rdesc (PTR x) = unwords ["PTR", x]

	ddesc (AbsDomain domain) = domain
	ddesc (RelDomain domain) = domain
	ddesc RootDomain = "@"

hostMap :: [Host] -> M.Map HostName Host
hostMap l = M.fromList $ zip (map hostName l) l

aliasMap :: [Host] -> M.Map HostName Host
aliasMap = M.fromList . concat .
	map (\h -> map (\aka -> (aka, h)) $ fromAliasesInfo $ fromInfo $ hostInfo h)

findHost :: [Host] -> HostName -> Maybe Host
findHost l hn = (findHostNoAlias l hn) <|> (findAlias l hn)

findHostNoAlias :: [Host] -> HostName -> Maybe Host
findHostNoAlias l hn = M.lookup hn (hostMap l)

findAlias :: [Host] -> HostName -> Maybe Host
findAlias l hn = M.lookup hn (aliasMap l)

getAddresses :: Info -> [IPAddr]
getAddresses = mapMaybe getIPAddr . S.toList . fromDnsInfo . fromInfo

hostAddresses :: HostName -> [Host] -> [IPAddr]
hostAddresses hn hosts = maybe [] (getAddresses . hostInfo) (findHost hosts hn)
