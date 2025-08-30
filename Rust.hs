module Rust where

import Base (OSNoInfo)
import Data.List (isInfixOf)
import Propellor
import Propellor.Base (doesFileExist, readProcess, (</>))
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

crateInstalled :: User -> [String] -> RevertableProperty OSNoInfo OSNoInfo
crateInstalled user@(User userName) crates =
  (mconcat $ installCrate <$> crates)
    <!> (tightenTargets $ mconcat $ uninstallCrate <$> crates)
  where
    home = "/home" </> userName

    installCrate crateName =
      check
        (crateNotInstalled crateName)
        ( userScriptProperty
            user
            [ "cargo install " <> crateName
            ]
        )
        `requires` rustInstalled user
        `describe` ("Installed crate " <> crateName)

    uninstallCrate crateName =
      ( userScriptProperty
          user
          [ "cargo install " <> crateName
          ]
      )
        `assume` MadeChange

    crateNotInstalled crateName =
      not
        . any (crateName `isInfixOf`)
        . lines
        <$> readProcess "/usr/bin/find" [home </> ".cargo" </> "registry"]

doesNotHaveRust :: IO Bool
doesNotHaveRust =
  not <$> doesFileExist "/opt/rust/bin/rustc"
