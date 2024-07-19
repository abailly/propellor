module Web where

import Propellor
import Propellor.Base (catchMaybeIO, hPutStr, liftIO, processTranscript, stderr, (</>))
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import Propellor.Property.LetsEncrypt (AgreeTOS (..), certFile, chainFile, fullChainFile, privKeyFile)
import qualified Propellor.Property.LetsEncrypt as LetsEncrypt
import qualified Propellor.Property.Nginx as Nginx
import Propellor.Utilities (doesFileExist)
import System.Posix (deviceID, fileID, fileMode, fileSize, getFileStatus, modificationTime)

letsEncryptAgree :: String -> LetsEncrypt.AgreeTOS
letsEncryptAgree email = LetsEncrypt.AgreeTOS (Just email)

httpsWebSite :: String -> [String] -> String -> Property DebianLike
httpsWebSite domainName nginxConfig email =
    propertyList ("Configured HTTPS website for " <> show domainName) $
        props
            & Nginx.siteEnabled domainName nginxConfig
                `onChange` selfSignedCert domainName
                `requires` File.hasContent "/etc/nginx/conf.d/connection-upgrade.conf" connectionUpgradeConf
            & letsEncryptCertsInstalled (letsEncryptAgree email) [domainName]
                `onChange` Nginx.reloaded
  where
    -- from https://futurestud.io/tutorials/nginx-how-to-fix-unknown-connection_upgrade-variable
    connectionUpgradeConf =
        [ "map $http_upgrade $connection_upgrade {  "
        , "    default upgrade;"
        , "    ''      close;"
        , "}"
        ]

letsEncryptCertsInstalled :: AgreeTOS -> [String] -> Property DebianLike
letsEncryptCertsInstalled (AgreeTOS memail) domains =
    prop `requires` Apt.installed ["certbot", "python3-certbot-nginx"]
  where
    prop :: Property UnixLike
    prop = property desc $ do
        startstats <- liftIO getstats
        (transcript, ok) <-
            liftIO $
                processTranscript "letsencrypt" params Nothing
        if ok
            then do
                endstats <- liftIO getstats
                if startstats /= endstats
                    then return MadeChange
                    else return NoChange
            else do
                liftIO $ hPutStr stderr transcript
                return FailedChange

    desc = "letsencrypt " ++ unwords domains
    params =
        [ "--nginx"
        , "--agree-tos"
        , case memail of
            Just email -> "--email=" ++ email
            Nothing -> "--register-unsafely-without-email"
        , "--noninteractive"
        , "--keep-until-expiring"
        , -- The list of domains may be changed, adding more, so
          -- always request expansion.
          "--expand"
        ]
            <> fmap ("--domain=" <>) domains

    getstats = mapM statcertfiles domains
    statcertfiles d =
        mapM
            statfile
            [ certFile d
            , privKeyFile d
            , chainFile d
            , fullChainFile d
            ]
    statfile f = catchMaybeIO $ do
        s <- getFileStatus f
        return (fileID s, deviceID s, fileMode s, fileSize s, modificationTime s)

selfSignedCert :: FilePath -> Property DebianLike
selfSignedCert domain =
    selfSignedGenerated
        `requires` File.dirExists ("/etc/letsencrypt/live" </> domain)
  where
    selfSignedGenerated :: Property DebianLike
    selfSignedGenerated = property desc $ do
        hasCert <- liftIO $ doesFileExist domainCertFile
        if hasCert
            then pure NoChange
            else genSelfSignedCert

    desc = "Self-signed cert for " <> domain
    genSelfSignedCert = do
        (transcript, ok) <- liftIO $ processTranscript "openssl" params Nothing
        if ok
            then do
                hasCert <- liftIO $ doesFileExist domainCertFile
                if hasCert
                    then return MadeChange
                    else return FailedChange
            else do
                liftIO $ hPutStr stderr transcript
                return FailedChange

    domainCertFile = "/etc/letsencrypt/live" </> domain </> "fullchain.pem"
    keyFile = "/etc/letsencrypt/live" </> domain </> "privkey.pem"
    params =
        [ "req"
        , "-x509"
        , "-nodes"
        , "-days"
        , "365"
        , "-newkey"
        , "rsa:4096"
        , "-subj"
        , "/C=FR/ST=France/L=Paris/CN=" <> domain
        , "-keyout"
        , keyFile
        , "-out"
        , domainCertFile
        ]
