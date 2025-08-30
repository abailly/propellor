module Rust where

import Base (OSNoInfo)
import Data.List (isInfixOf)
import Propellor
import Propellor.Base (doesFileExist, readProcess)
import qualified Propellor.Property.Apt as Apt

rustInstalled :: User -> Property OSNoInfo
rustInstalled user =
  check
    doesNotHaveRust
    ( userScriptProperty
        user
        [ "curl -sSf https://sh.rustup.rs | sudo RUSTUP_HOME=/opt/rust CARGO_HOME=/opt/rust sh -s -- --no-modify-path -y",
          "echo 'export RUSTUP_HOME=/opt/rust' | sudo tee -a /etc/profile.d/rust.sh",
          "echo 'export PATH=$PATH:/opt/rust/bin' | sudo tee -a /etc/profile.d/rust.sh"
        ]
    )
    `requires` Apt.installed ["gcc", "build-essential", "m4", "pkgconf", "libssl-dev"]
    `describe` "Rustup toolchain installed"

crateInstalled :: User -> String -> Property OSNoInfo
crateInstalled user@(User userName) crateName =
  check
    crateNotInstalled
    ( userScriptProperty
        user
        [ "cargo install " <> crateName
        ]
    )
    `requires` rustInstalled user
    `describe` ("Installed crate " <> crateName)
  where
    crateNotInstalled = any (crateName `isInfixOf`) . lines <$> readProcess "su" [ "-c", "/opt/rust/bin/cargo install --list", userName ]

doesNotHaveRust :: IO Bool
doesNotHaveRust =
  not <$> doesFileExist "/opt/rust/bin/rustc"
