-- | Maintainer: Arnaud Bailly <arnaud.oqube@gmail.com>
--
-- Properties for configuring firewall (iptables) rules

module Propellor.Property.Firewall (
	rule,
	installed,
	Chain(..),
	Table(..),
	Target(..),
	Proto(..),
	Rules(..),
	ConnectionState(..),
	ICMPTypeMatch(..),
	TCPFlag(..),
	Frequency(..),
	IPWithMask(..),
	fromIPWithMask
) where

import Data.Monoid
import Data.Char
import Data.List

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Network as Network

installed :: Property DebianLike
installed = Apt.installed ["iptables"]

rule :: Chain -> Table -> Target -> Rules -> Property Linux
rule c tb tg rs = property ("firewall rule: " <> show r) addIpTable
  where
	r = Rule c tb tg rs
	addIpTable = liftIO $ do
		let args = toIpTable r
		exist <- boolSystem "iptables" (chk args)
		if exist
			then return NoChange
			else toResult <$> boolSystem "iptables" (add args)
	add params = Param "-A" : params
	chk params = Param "-C" : params

toIpTable :: Rule -> [CommandParam]
toIpTable r =  map Param $
	fromChain (ruleChain r) :
	toIpTableArg (ruleRules r) ++
	["-t", fromTable (ruleTable r), "-j", fromTarget (ruleTarget r)]

toIpTableArg :: Rules -> [String]
toIpTableArg Everything = []
toIpTableArg (Proto proto) = ["-p", map toLower $ show proto]
toIpTableArg (DPort port) = ["--dport", fromPort port]
toIpTableArg (DPortRange (portf, portt)) =
	["--dport", fromPort portf ++ ":" ++ fromPort portt]
toIpTableArg (InIFace iface) = ["-i", iface]
toIpTableArg (OutIFace iface) = ["-o", iface]
toIpTableArg (Ctstate states) =
	[ "-m"
	, "conntrack"
	, "--ctstate", intercalate "," (map show states)
	]
toIpTableArg (ICMPType i) =
	[ "-m"
	, "icmp"
	, "--icmp-type", fromICMPTypeMatch i
	]
toIpTableArg (RateLimit f) =
	[ "-m"
	, "limit"
	, "--limit", fromFrequency f
	]
toIpTableArg (TCPFlags m c) =
	[ "-m"
	, "tcp"
	, "--tcp-flags"
	, intercalate "," (map show m)
	, intercalate "," (map show c)
	]
toIpTableArg TCPSyn = ["--syn"]
toIpTableArg (GroupOwner (Group g)) =
	[ "-m"
	, "owner"
	, "--gid-owner"
	, g
	]
toIpTableArg (Source ipwm) =
	[ "-s"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (Destination ipwm) =
	[ "-d"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (NotDestination ipwm) =
	[ "!"
	, "-d"
	, intercalate "," (map fromIPWithMask ipwm)
	]
toIpTableArg (NatDestination ip mport) =
	[ "--to-destination"
	, fromIPAddr ip ++ maybe "" (\p -> ":" ++ fromPort p) mport
	]
toIpTableArg (r :- r') = toIpTableArg r <> toIpTableArg r'

data IPWithMask = IPWithNoMask IPAddr | IPWithIPMask IPAddr IPAddr | IPWithNumMask IPAddr Int
	deriving (Eq, Show)

fromIPWithMask :: IPWithMask -> String
fromIPWithMask (IPWithNoMask ip) = fromIPAddr ip
fromIPWithMask (IPWithIPMask ip ipm) = fromIPAddr ip ++ "/" ++ fromIPAddr ipm
fromIPWithMask (IPWithNumMask ip m) = fromIPAddr ip ++ "/" ++ show m

data Rule = Rule
	{ ruleChain  :: Chain
	, ruleTable  :: Table
	, ruleTarget :: Target
	, ruleRules  :: Rules
	} deriving (Eq, Show)

data Table = Filter | Nat | Mangle | Raw | Security
	deriving (Eq, Show)

fromTable :: Table -> String
fromTable Filter = "filter"
fromTable Nat = "nat"
fromTable Mangle = "mangle"
fromTable Raw = "raw"
fromTable Security = "security"

data Target = ACCEPT | REJECT | DROP | LOG | TargetCustom String
	deriving (Eq, Show)

fromTarget :: Target -> String
fromTarget ACCEPT = "ACCEPT"
fromTarget REJECT = "REJECT"
fromTarget DROP = "DROP"
fromTarget LOG = "LOG"
fromTarget (TargetCustom t) = t

data Chain = INPUT | OUTPUT | FORWARD | PREROUTING | POSTROUTING | ChainCustom String
	deriving (Eq, Show)

fromChain :: Chain -> String
fromChain INPUT = "INPUT"
fromChain OUTPUT = "OUTPUT"
fromChain FORWARD = "FORWARD"
fromChain PREROUTING = "PREROUTING"
fromChain POSTROUTING = "POSTROUTING"
fromChain (ChainCustom c) = c

data Proto = TCP | UDP | ICMP
	deriving (Eq, Show)

data ConnectionState = ESTABLISHED | RELATED | NEW | INVALID
	deriving (Eq, Show)

data ICMPTypeMatch = ICMPTypeName String | ICMPTypeCode Int
	deriving (Eq, Show)

fromICMPTypeMatch :: ICMPTypeMatch -> String
fromICMPTypeMatch (ICMPTypeName t) = t
fromICMPTypeMatch (ICMPTypeCode c) = show c

data Frequency = NumBySecond Int
	deriving (Eq, Show)

fromFrequency :: Frequency -> String
fromFrequency (NumBySecond n) = show n ++ "/second"

type TCPFlagMask = [TCPFlag]

type TCPFlagComp = [TCPFlag]

data TCPFlag = SYN | ACK | FIN | RST | URG | PSH | ALL | NONE
	deriving (Eq, Show)

data Rules
	= Everything
	| Proto Proto
	-- ^There is actually some order dependency between proto and port so this should be a specific
	-- data type with proto + ports
	| DPort Port
	| DPortRange (Port, Port)
	| InIFace Network.Interface
	| OutIFace Network.Interface
	| Ctstate [ ConnectionState ]
	| ICMPType ICMPTypeMatch
	| RateLimit Frequency
	| TCPFlags TCPFlagMask TCPFlagComp
	| TCPSyn
	| GroupOwner Group
	| Source [ IPWithMask ]
	| Destination [ IPWithMask ]
	| NotDestination [ IPWithMask ]
	| NatDestination IPAddr (Maybe Port)
	| Rules :- Rules   -- ^Combine two rules
	deriving (Eq, Show)

infixl 0 :-

instance Monoid Rules where
	mempty  = Everything
	mappend = (:-)
