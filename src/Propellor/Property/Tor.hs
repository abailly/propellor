{-# LANGUAGE TypeFamilies #-}

module Propellor.Property.Tor where

import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.Service as Service
import qualified Propellor.Property.ConfFile as ConfFile
import Utility.FileMode
import Utility.DataUnits

import System.Posix.Files
import Data.Char
import Data.List

type HiddenServiceName = String

type NodeName = String

-- | Sets up a tor bridge. (Not a relay or exit node.)
--
-- Uses port 443
isBridge :: Property DebianLike
isBridge = configured
	[ ("BridgeRelay", "1")
	, ("Exitpolicy", "reject *:*")
	, ("ORPort", "443")
	]
	`describe` "tor bridge"
	`requires` server

-- | Sets up a tor relay.
--
-- Uses port 443
isRelay :: Property DebianLike
isRelay = configured
	[ ("BridgeRelay", "0")
	, ("Exitpolicy", "reject *:*")
	, ("ORPort", "443")
	]
	`describe` "tor relay"
	`requires` server

-- | Makes the tor node be named, with a known private key.
--
-- This can be moved to a different IP without needing to wait to
-- accumulate trust.
named :: NodeName -> Property (HasInfo + DebianLike)
named n = configured [("Nickname", n')]
	`describe` ("tor node named " ++ n')
	`requires` torPrivKey (Context ("tor " ++ n))
  where
	n' = saneNickname n

torPrivKey :: Context -> Property (HasInfo + DebianLike)
torPrivKey context = f `File.hasPrivContent` context
	`onChange` File.ownerGroup f user (userGroup user)
	`requires` torPrivKeyDirExists
  where
	f = torPrivKeyDir </> "secret_id_key"

torPrivKeyDirExists :: Property DebianLike
torPrivKeyDirExists = File.dirExists torPrivKeyDir
	`onChange` setperms
	`requires` installed
  where
	setperms = File.ownerGroup torPrivKeyDir user (userGroup user)
		`before` File.mode torPrivKeyDir 0O2700

torPrivKeyDir :: FilePath
torPrivKeyDir = "/var/lib/tor/keys"

-- | A tor server (bridge, relay, or exit)
-- Don't use if you just want to run tor for personal use.
server :: Property DebianLike
server = configured [("SocksPort", "0")]
	`requires` installed
	`requires` Apt.installed ["ntp"]
	`describe` "tor server"

installed :: Property DebianLike
installed = Apt.installed ["tor"]

-- | Specifies configuration settings. Any lines in the config file
-- that set other values for the specified settings will be removed,
-- while other settings are left as-is. Tor is restarted when
-- configuration is changed.
configured :: [(String, String)] -> Property DebianLike
configured settings = File.fileProperty "tor configured" go mainConfig
	`onChange` restarted
  where
	ks = map fst settings
	go ls = sort $ map toconfig $
		filter (\(k, _) -> k `notElem` ks) (map fromconfig ls)
		++ settings
	toconfig (k, v) = k ++ " " ++ v
	fromconfig = separate (== ' ')

data BwLimit
	= PerSecond String
	| PerDay String
	| PerMonth String

-- | Limit incoming and outgoing traffic to the specified
-- amount each.
--
-- For example, PerSecond "30 kibibytes" is the minimum limit
-- for a useful relay.
bandwidthRate :: BwLimit -> Property DebianLike
bandwidthRate (PerSecond s) = bandwidthRate' s 1
bandwidthRate (PerDay s) = bandwidthRate' s (24*60*60)
bandwidthRate (PerMonth s) = bandwidthRate' s (31*24*60*60)

bandwidthRate' :: String -> Integer -> Property DebianLike
bandwidthRate' s divby = case readSize dataUnits s of
	Just sz -> let v = show (sz `div` divby) ++ " bytes"
		in configured [("BandwidthRate", v)]
			`describe` ("tor BandwidthRate " ++ v)
	Nothing -> property ("unable to parse " ++ s) noChange

hiddenServiceAvailable :: HiddenServiceName -> Int -> Property DebianLike
hiddenServiceAvailable hn port = hiddenServiceHostName $ hiddenService hn port
  where
	hiddenServiceHostName p =  adjustPropertySatisfy p $ \satisfy -> do
		r <- satisfy
		h <- liftIO $ readFile (varLib </> hn </> "hostname")
		warningMessage $ unwords ["hidden service hostname:", h]
		return r

hiddenService :: HiddenServiceName -> Int -> Property DebianLike
hiddenService hn port = ConfFile.adjustSection
	(unwords ["hidden service", hn, "available on port", show port])
	(== oniondir)
	(not . isPrefixOf "HiddenServicePort")
	(const [oniondir, onionport])
	(++ [oniondir, onionport])
	mainConfig
	`onChange` restarted
  where
	oniondir = unwords ["HiddenServiceDir", varLib </> hn]
	onionport = unwords ["HiddenServicePort", show port, "127.0.0.1:" ++ show port]

hiddenServiceData :: IsContext c => HiddenServiceName -> c -> Property (HasInfo + DebianLike)
hiddenServiceData hn context = combineProperties desc $ props
	& installonion "hostname"
	& installonion "private_key"
  where
	desc = unwords ["hidden service data available in", varLib </> hn]
	installonion :: FilePath -> Property (HasInfo + DebianLike)
	installonion f = withPrivData (PrivFile $ varLib </> hn </> f) context $ \getcontent ->
		property' desc $ \w -> getcontent $ install w $ varLib </> hn </> f
	install w f privcontent = ifM (liftIO $ doesFileExist f)
		( noChange
		, ensureProperty w $ propertyList desc $ toProps
			[ property desc $ makeChange $ do
				createDirectoryIfMissing True (takeDirectory f)
				writeFileProtected f (unlines (privDataLines privcontent))
			, File.mode (takeDirectory f) $ combineModes
				[ownerReadMode, ownerWriteMode, ownerExecuteMode]
			, File.ownerGroup (takeDirectory f) user (userGroup user)
			, File.ownerGroup f user (userGroup user)
			]
		)

restarted :: Property DebianLike
restarted = Service.restarted "tor"

mainConfig :: FilePath
mainConfig = "/etc/tor/torrc"

varLib :: FilePath
varLib = "/var/lib/tor"

varRun :: FilePath
varRun = "/var/run/tor"

user :: User
user = User "debian-tor"

type NickName = String

-- | Convert String to a valid tor NickName.
saneNickname :: String -> NickName
saneNickname s
	| null n = "unnamed"
	| otherwise = n
  where
	legal c = isNumber c || isAsciiUpper c || isAsciiLower c
	n = take 19 $ filter legal s
