-- | Maintainer: Félix Sipma <felix+propellor@gueux.org>
--
-- Properties for the Unbound caching DNS server

module Propellor.Property.Unbound
	( installed
	, restarted
	, reloaded
	, UnboundSection
	, UnboundZone
	, UnboundHost
	, UnboundSetting
	, UnboundValue
	, UnboundKey
	, ConfSection
	, ZoneType
	, cachingDnsServer
	) where

import Propellor.Base
import Propellor.Property.File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service

import Data.List (find)


type ConfSection = String

type UnboundSetting = (UnboundKey, UnboundValue)

type UnboundSection = (ConfSection, [UnboundSetting])

type UnboundZone = (BindDomain, ZoneType)

type UnboundHost = (BindDomain, Record)

type UnboundKey = String

type UnboundValue = String

type ZoneType = String

installed :: Property DebianLike
installed = Apt.installed ["unbound"]

restarted :: Property DebianLike
restarted = Service.restarted "unbound"

reloaded :: Property DebianLike
reloaded = Service.reloaded "unbound"

dValue :: BindDomain -> String
dValue (RelDomain d) = d
dValue (AbsDomain d) = d ++ "."
dValue (RootDomain) = "@"

sectionHeader :: ConfSection -> String
sectionHeader header = header ++ ":"

config :: FilePath
config = "/etc/unbound/unbound.conf.d/propellor.conf"

-- | Provided a [UnboundSection], a [UnboundZone] and a [UnboundHost],
-- cachingDnsServer ensure unbound is configured accordingly.
--
-- Example property:
--
-- > cachingDnsServer
-- >      [ ("remote-control", [("control-enable", "no")]
-- >      , ("server",
-- >      	[ ("interface", "0.0.0.0")
-- >      	, ("access-control", "192.168.1.0/24 allow")
-- >      	, ("do-tcp", "no")
-- >      	])
-- >      [ (AbsDomain "example.com", "transparent")
-- >      , (AbsDomain $ reverseIP $ IPv4 "192.168.1", "static")
-- >      ]
-- >      [ (AbsDomain "example.com", Address $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "myhost.example.com", Address $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "myrouter.example.com", Address $ IPv4 "192.168.1.1")
-- >      , (AbsDomain "www.example.com", Address $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "example.com", MX 10 "mail.example.com")
-- >      , (AbsDomain "mylaptop.example.com", Address $ IPv4 "192.168.1.2")
-- >      -- ^ connected via ethernet
-- >      , (AbsDomain "mywifi.example.com", Address $ IPv4 "192.168.2.1")
-- >      , (AbsDomain "mylaptop.example.com", Address $ IPv4 "192.168.2.2")
-- >      -- ^ connected via wifi, use round robin
-- >      , (AbsDomain "myhost.example.com", PTR $ reverseIP $ IPv4 "192.168.1.2")
-- >      , (AbsDomain "myrouter.example.com", PTR $ reverseIP $ IPv4 "192.168.1.1")
-- >      , (AbsDomain "mylaptop.example.com", PTR $ reverseIP $ IPv4 "192.168.1.2")
-- >      ]
cachingDnsServer :: [UnboundSection] -> [UnboundZone] -> [UnboundHost] -> Property DebianLike
cachingDnsServer sections zones hosts =
	config `hasContent` (comment : otherSections ++ serverSection)
	`onChange` restarted
  where
	comment = "# deployed with propellor, do not modify"
	serverSection = genSection (fromMaybe ("server", []) $ find ((== "server") . fst) sections)
		++ map genZone zones
		++ map (uncurry genRecord') hosts
	otherSections = foldr ((++) . genSection) [] $ filter ((/= "server") . fst) sections

genSection :: UnboundSection -> [Line]
genSection (section, settings) = sectionHeader section : map genSetting settings

genSetting :: UnboundSetting -> Line
genSetting (key, value) = "    " ++ key ++ ": " ++ value

genZone :: UnboundZone -> Line
genZone (dom, zt) = "    local-zone: \"" ++ dValue dom ++ "\" " ++ zt

genRecord' :: BindDomain -> Record -> Line
genRecord' dom r = "    local-data: \"" ++ fromMaybe "" (genRecord dom r) ++ "\""

genRecord :: BindDomain -> Record -> Maybe String
genRecord dom (Address addr) = Just $ genAddressNoTtl dom addr
genRecord dom (MX priority dest) = Just $ genMX dom priority dest
genRecord dom (PTR revip) = Just $ genPTR dom revip
genRecord _ (CNAME _) = Nothing
genRecord _ (NS _) = Nothing
genRecord _ (TXT _) = Nothing
genRecord _ (SRV _ _ _ _) = Nothing
genRecord _ (SSHFP _ _ _) = Nothing
genRecord _ (INCLUDE _) = Nothing

genAddressNoTtl :: BindDomain -> IPAddr -> String
genAddressNoTtl dom = genAddress dom Nothing

genAddress :: BindDomain -> Maybe Int -> IPAddr -> String
genAddress dom ttl addr = case addr of
	IPv4 _ -> genAddress' "A" dom ttl addr
	IPv6 _ -> genAddress' "AAAA" dom ttl addr

genAddress' :: String -> BindDomain -> Maybe Int -> IPAddr -> String
genAddress' recordtype dom ttl addr = dValue dom ++ " " ++ maybe "" (\ttl' -> show ttl' ++ " ") ttl ++ "IN " ++ recordtype ++ " " ++ fromIPAddr addr

genMX :: BindDomain -> Int -> BindDomain -> String
genMX dom priority dest = dValue dom ++ " " ++ "MX" ++ " " ++ show priority ++ " " ++ dValue dest

genPTR :: BindDomain -> ReverseIP -> String
genPTR dom revip = revip ++ ". " ++ "PTR" ++ " " ++ dValue dom
