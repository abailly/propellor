module Wireguard where

import Base (OSNoInfo)
import Propellor (Property, RevertableProperty, propertyList, props, (&), (<!>))
import qualified Propellor.Property.Apt as Apt

installed :: RevertableProperty OSNoInfo OSNoInfo
installed =
  setupWireguard <!> teardownWireguard
 where
  setupWireguard :: Property OSNoInfo
  setupWireguard =
    propertyList "Wireguard installed" $
      props
        & Apt.installed ["wireguard"]

  teardownWireguard :: Property OSNoInfo
  teardownWireguard =
    propertyList "Wireguard removed" $
      props
        & Apt.removed ["wireguard"]
