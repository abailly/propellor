{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wireguard where

import Base (OS)
import Data.String (IsString)
import Propellor (
  PrivData (..),
  PrivDataField (..),
  Property,
  RevertableProperty,
  assume,
  ensureProperty,
  hostContext,
  property',
  propertyList,
  props,
  requires,
  scriptProperty,
  tightenTargets,
  (&),
  (<!>),
 )
import Propellor.Base (Result (MadeChange), withPrivData)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

newtype IFace = IFace String
  deriving newtype (Show, Eq, IsString)

installed :: RevertableProperty OS OS
installed =
  setupWireguard <!> teardownWireguard
 where
  setupWireguard :: Property OS
  setupWireguard =
    propertyList "Wireguard installed" $
      props
        & Apt.installed ["wireguard"]
        & interfaceConfigured wg0

  wg0 = IFace "wg0"

  interfaceConfigured :: IFace -> Property OS
  interfaceConfigured (IFace ifName) =
    withPrivData (PrivFile "wireguard-key") hostContext $ \getPrivData ->
      property' ("wireguard interface " <> ifName) $ \w ->
        getPrivData $ \(PrivData privateKey) ->
          ensureProperty w $
            scriptProperty ["wg-quick up " <> ifName]
              `assume` MadeChange
              `requires` File.hasContent ("/etc/wireguard/" <> ifName <> ".conf") (configFile privateKey)

  teardownWireguard :: Property OS
  teardownWireguard =
    tightenTargets $
      propertyList "Wireguard removed" $
        props
          & Apt.removed ["wireguard"]

  configFile privateKey =
    [ "[Interface]"
    , "Address = 10.10.0.1/16"
    , "SaveConfig = true"
    , "PrivateKey = " <> privateKey
    , "ListenPort = 51820"
    ]
