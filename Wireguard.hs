{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Wireguard where

import Base (OS)
import Data.String (IsString)
import Data.Word (Word16)
import Propellor (
  PrivData (..),
  PrivDataField (..),
  Property,
  RevertableProperty,
  assume,
  check,
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
import Propellor.Base (Result (MadeChange), doesFileExist, readProcess, withPrivData)
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File

newtype IFace = IFace String
  deriving newtype (Show, Eq, IsString)

serverInstalled :: RevertableProperty OS OS
serverInstalled =
  setupWireguard <!> teardownWireguard
 where
  setupWireguard :: Property OS
  setupWireguard =
    propertyList "Wireguard installed" $
      props & interfaceConfigured wg0

  wg0 = IFace "wg0"

  interfaceConfigured :: IFace -> Property OS
  interfaceConfigured (IFace ifName) =
    withPrivData (PrivFile "wireguard-key") hostContext $ \getPrivData ->
      property' ("wireguard interface " <> ifName) $ \w ->
        getPrivData $ \(PrivData privateKey) ->
          ensureProperty w $
            check (not <$> doesFileExist fileName) (scriptProperty ["wg-quick up " <> ifName])
              `assume` MadeChange
              `requires` File.hasContent fileName (configuration privateKey)
              `requires` Apt.installed ["wireguard"]
   where
    fileName = "/etc/wireguard/" <> ifName <> ".conf"

  teardownWireguard :: Property OS
  teardownWireguard =
    tightenTargets $
      propertyList "Wireguard removed" $
        props
          & Apt.removed ["wireguard"]

  configuration privateKey =
    [ "[Interface]"
    , "Address = 10.10.0.1/16"
    , "SaveConfig = true"
    , "PrivateKey = " <> privateKey
    , "ListenPort = 51820"
    ]

newtype WgPublicKey = WgPublicKey String
  deriving newtype (Show, Eq, IsString)

data Endpoint = Endpoint String Word16
  deriving (Show, Eq)

clientInstalled :: WgPublicKey -> Endpoint -> Property OS
clientInstalled serverPublicKey serverEndpoint =
  setupWireguard
 where
  setupWireguard :: Property OS
  setupWireguard =
    withPrivData (PrivFile "wireguard-key") hostContext $ \getPrivData ->
      property' "Wireguard client installed" $ \w ->
        getPrivData $ \(PrivData privateKey) ->
          ensureProperty w $
            check (interfaceUp ifName) (scriptProperty ["wg-quick up " <> ifName])
              `assume` MadeChange
              `requires` File.hasContent fileName (configuration privateKey)
              `requires` Apt.installed ["wireguard"]
   where
    ifName = "wg0"
    fileName = "/etc/wireguard/" <> ifName <> ".conf"

    configuration privateKey =
      [ "[Interface]"
      , "Address = 10.10.0.3/16"
      , "SaveConfig = true"
      , "PrivateKey = " <> privateKey
      ]
        <> peerConfiguration serverPublicKey serverEndpoint

    peerConfiguration :: WgPublicKey -> Endpoint -> [String]
    peerConfiguration (WgPublicKey pubKey) (Endpoint host port) =
      [ "[Peer]"
      , "PublicKey = " <> pubKey
      , "AllowedIPs = 10.10.0.0/16"
      , "Endpoint = " <> host <> ":" <> show port
      ]

    interfaceUp :: String -> IO Bool
    interfaceUp ifaceName = do
      out <- lines <$> readProcess "wg" ["show", "interfaces"]
      pure $
        (not (null out) && ((ifaceName `elem`) . words . head $ out))
