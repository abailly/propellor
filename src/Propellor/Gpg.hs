module Propellor.Gpg where

import System.IO
import Data.Maybe
import Data.List.Utils
import Control.Monad
import Control.Applicative
import Prelude

import Propellor.PrivData.Paths
import Propellor.Message
import Propellor.Git.Config
import Utility.SafeCommand
import Utility.Process
import Utility.Process.NonConcurrent
import Utility.Monad
import Utility.Misc
import Utility.Tmp
import Utility.FileSystemEncoding
import Utility.Env
import Utility.Directory

type KeyId = String

getGpgBin :: IO String
getGpgBin = do
	gitGpgBin <- getGitConfigValue "gpg.program"
	case gitGpgBin of
		Nothing -> getEnvDefault "GNUPGBIN" "gpg"
		Just b -> return b

-- Lists the keys in propellor's keyring.
listPubKeys :: IO [KeyId]
listPubKeys = do
	keyring <- privDataKeyring
	map fst <$> listKeys ("--list-public-keys" : useKeyringOpts keyring)

listSecretKeys :: IO [(KeyId, String)]
listSecretKeys = listKeys ["--list-secret-keys"]

listKeys :: [String] -> IO [(KeyId, String)]
listKeys ps = do
	gpgbin <- getGpgBin
	parse . lines <$> readProcess gpgbin listopts
  where
	listopts = ps ++ ["--with-colons"]
	parse = mapMaybe (keyIdField . split ":")
	keyIdField (t:_:_:_:f:_:_:_:_:n:_)
		| t == "pub" || t == "sec" = Just (f, n)
	keyIdField _ = Nothing

useKeyringOpts :: FilePath -> [String]
useKeyringOpts keyring =
	[ "--options"
	, "/dev/null"
	, "--no-default-keyring"
	, "--keyring", keyring
	]

addKey :: KeyId -> IO ()
addKey keyid = do
	gpgbin <- getGpgBin
	keyring <- privDataKeyring
	exitBool =<< allM (uncurry actionMessage)
		[ ("adding key to propellor's keyring", addkeyring keyring gpgbin)
		, ("staging propellor's keyring", gitAdd keyring)
		, ("updating encryption of any privdata", reencryptPrivData)
		, ("configuring git commit signing to use key", gitconfig gpgbin)
		, ("committing changes", gitCommitKeyRing "add-key")
		]
  where
	addkeyring keyring' gpgbin' = do
		createDirectoryIfMissing True privDataDir
		boolSystem "sh"
			[ Param "-c"
			, Param $ gpgbin' ++ " --export " ++ keyid ++ " | gpg " ++
				unwords (useKeyringOpts keyring' ++ ["--import"])
			]

	gitconfig gpgbin' = ifM (snd <$> processTranscript gpgbin' ["--list-secret-keys", keyid] Nothing)
		( boolSystem "git"
			[ Param "config"
			, Param "user.signingkey"
			, Param keyid
			]
		, do
			warningMessage $ "Cannot find a secret key for key " ++ keyid ++ ", so not configuring git user.signingkey to use this key."
			return True
		)

rmKey :: KeyId -> IO ()
rmKey keyid = do
	gpgbin <- getGpgBin
	keyring <- privDataKeyring
	exitBool =<< allM (uncurry actionMessage)
		[ ("removing key from propellor's keyring", rmkeyring keyring gpgbin)
		, ("staging propellor's keyring", gitAdd keyring)
		, ("updating encryption of any privdata", reencryptPrivData)
		, ("configuring git commit signing to not use key", gitconfig)
		, ("committing changes", gitCommitKeyRing "rm-key")
		]
  where
	rmkeyring keyring' gpgbin' = boolSystem gpgbin' $
		(map Param (useKeyringOpts keyring')) ++
		[ Param "--batch"
		, Param "--yes"
		, Param "--delete-key", Param keyid
		]

	gitconfig = ifM ((==) (keyid++"\n", True) <$> processTranscript "git" ["config", "user.signingkey"] Nothing)
		( boolSystem "git"
			[ Param "config"
			, Param "--unset"
			, Param "user.signingkey"
			]
		, return True
		)

reencryptPrivData :: IO Bool
reencryptPrivData = do
	f <- privDataFile
	ifM (doesFileExist f)
		( do
			gpgEncrypt f =<< gpgDecrypt f
			gitAdd f
		, return True
		)

gitAdd :: FilePath -> IO Bool
gitAdd f = boolSystem "git"
	[ Param "add"
	, File f
	]

gitCommitKeyRing :: String -> IO Bool
gitCommitKeyRing action = do
	keyring <- privDataKeyring
	privdata <- privDataFile
	-- Commit explicitly the keyring and privdata files, as other
	-- changes may be staged by the user and shouldn't be committed.
	tocommit <- filterM doesFileExist [ privdata, keyring]
	gitCommit (Just ("propellor " ++ action)) (map File tocommit)

-- Adds --gpg-sign if there's a keyring.
gpgSignParams :: [CommandParam] -> IO [CommandParam]
gpgSignParams ps = do
	keyring <- privDataKeyring
	ifM (doesFileExist keyring)
		( return (ps ++ [Param "--gpg-sign"])
		, return ps
		)

-- Automatically sign the commit if there'a a keyring.
gitCommit :: Maybe String -> [CommandParam] -> IO Bool
gitCommit msg ps = do
	let ps' = Param "commit" : ps ++
		maybe [] (\m -> [Param "-m", Param m]) msg
	ps'' <- gpgSignParams ps'
	boolSystemNonConcurrent "git" ps''

gpgDecrypt :: FilePath -> IO String
gpgDecrypt f = do
	gpgbin <- getGpgBin
	ifM (doesFileExist f)
		( writeReadProcessEnv gpgbin ["--decrypt", f] Nothing Nothing (Just fileEncoding)
		, return ""
		)

-- Encrypt file to all keys in propellor's keyring.
gpgEncrypt :: FilePath -> String -> IO ()
gpgEncrypt f s = do
	gpgbin <- getGpgBin
	keyids <- listPubKeys
	let opts =
		[ "--default-recipient-self"
		, "--armor"
		, "--encrypt"
		, "--trust-model", "always"
		] ++ concatMap (\k -> ["--recipient", k]) keyids
	encrypted <- writeReadProcessEnv gpgbin opts Nothing (Just writer) Nothing
	viaTmp writeFile f encrypted
  where
	writer h = do
		fileEncoding h
		hPutStr h s
