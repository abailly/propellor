{-# LANGUAGE DataKinds #-}

module CardanoUp where

import Base (OSNoInfo)
import Propellor (Property, RevertableProperty, TightenTargets (tightenTargets), changesFileContent, check, cmdProperty, describe, propertyList, props, (&), (<!>))
import Propellor.Base (combineModes, doesFileExist, readProcessEnv)
import qualified Propellor.Property.File as File
import System.Posix.Files (groupModes, otherExecuteMode, otherReadMode, ownerModes)
import Prelude

-- | Install cardano-up system wide
install :: RevertableProperty OSNoInfo OSNoInfo
install =
  setupCardanoUp <!> teardownCardanoUp
  where
    exePath = "/usr/local/bin/cardano-up"

    teardownCardanoUp :: Property OSNoInfo
    teardownCardanoUp =
      tightenTargets $
        propertyList "Remove cardano-up" $
          props
            & File.notPresent exePath

    setupCardanoUp :: Property OSNoInfo
    setupCardanoUp =
      propertyList "Install cardano-up" $
        props
          & downloadCardanoUp
          & exePath
            `File.mode` combineModes [ownerModes, groupModes, otherReadMode, otherExecuteMode]

    downloadCardanoUp :: Property OSNoInfo
    downloadCardanoUp =
      tightenTargets
        $ propertyList
          "Cardano-up installed in /usr/local/bin"
        $ props
          & check
            shouldDownload
            ( cmdProperty
                "curl"
                ["-o", exePath, "-L", "https://github.com/blinklabs-io/cardano-up/releases/download/" <> cardanoUpVersion <> "/cardano-up-" <> cardanoUpVersion <> "-linux-amd64"]
                `changesFileContent` exePath
            )
            `describe` ("Cardano up " <> cardanoUpVersion <> " archive downloaded")

    shouldDownload = do
      hasFile <- doesFileExist exePath
      if hasFile
        then
          not
            . (cardanoUpVersion `elem`)
            . words
            . head
            . lines
            <$> readProcessEnv exePath ["version"] Nothing
        else pure True

    cardanoUpVersion = "v0.14.1"
