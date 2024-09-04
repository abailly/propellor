module Radicle where

import Base (OSNoInfo)
import Propellor
import qualified Propellor.Property.User as User

seedInstalled :: String -> RevertableProperty OSNoInfo OSNoInfo
seedInstalled userName =
    setupRadicleSeed <!> teardownRadicleSeed
  where
    setupRadicleSeed =
        propertyList "Radicle seed installed" $
            props
                & User.systemAccountFor' (User userName) (Just "/home/seed") (Just $ Group userName)

    teardownRadicleSeed =
        tightenTargets $
            propertyList "Radicle seed removed" $
                props
                    & User.nuked (User userName) User.YesReallyDeleteHome
