{-# LANGUAGE DataKinds #-}

module User where

import Base (OS)
import Propellor.Base
import qualified Propellor.Property.File as File
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
import qualified Propellor.Types.MetaTypes as MetaTypes
import qualified Propellor.Types.OS as OS

commonUserSetup ::
  OS.User ->
  Property
    ( MetaTypes.MetaTypes
        [ MetaTypes.WithInfo,
          MetaTypes.Targeting OS.OSDebian,
          MetaTypes.Targeting OS.OSBuntish
        ]
    )
commonUserSetup user =
  propertyList ("Setup user " <> show user) $
    props
      & User.accountFor user
      & Ssh.authorizedKey user ""
      & Ssh.authorizedKeys user hostContext
      & Ssh.userKeyAt
        Nothing
        user
        hostContext
        ( Ssh.SshEd25519,
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIERjBICdoL0S4dU+HgevTutHF0QajK/qEN1iHKgeU7+T Remote curry user's key"
        )
      & Sudo.enabledFor user
      & User.hasGroup user systemdJournal
      & tmuxSetup user
  where
    systemdJournal = Group "systemd-journal"

tmuxSetup :: OS.User -> Property OS
tmuxSetup user =
  property' ("Configure tmux for user " <> show user) $ \w -> do
    dir <- liftIO $ User.homedir user
    group <- liftIO $ User.primaryGroup user
    ensureProperty w
      ( File.hasContent (tmuxConfFile dir) tmuxConfig
          <> File.ownerGroup (tmuxConfFile dir) user group
      )

tmuxConfFile :: FilePath -> FilePath
tmuxConfFile = (</> ".tmux.conf")

tmuxConfig :: [String]
tmuxConfig =
  [ "set -g prefix C-b",
    "set -g default-terminal \"screen-256color\"",
    "set-window-option -g xterm-keys on",
    "set -g mouse on",
    "set -s escape-time 0",
    "set-option -g default-shell /bin/bash"
  ]
