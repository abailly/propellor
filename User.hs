{-# LANGUAGE DataKinds #-}

module User where

import Propellor ((&))
import Propellor.Base (Group (Group), Property, propertyList, props)
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Sudo as Sudo
import qualified Propellor.Property.User as User
import qualified Propellor.Types.MetaTypes as MetaTypes
import qualified Propellor.Types.OS as OS
import Propellor.Types.PrivData (hostContext)

commonUserSetup ::
    OS.User ->
    Property
        ( MetaTypes.MetaTypes
            [ MetaTypes.WithInfo
            , MetaTypes.Targeting OS.OSDebian
            , MetaTypes.Targeting OS.OSBuntish
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
                ( Ssh.SshEd25519
                , "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIERjBICdoL0S4dU+HgevTutHF0QajK/qEN1iHKgeU7+T Remote curry user's key"
                )
            & Sudo.enabledFor user
            & User.hasGroup user systemdJournal
  where
    systemdJournal = Group "systemd-journal"
