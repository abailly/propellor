module Propellor.Property.SiteSpecific.Branchable where

import Propellor.Base
import qualified Propellor.Property.Apt as Apt
import qualified Propellor.Property.File as File
import qualified Propellor.Property.User as User
import qualified Propellor.Property.Ssh as Ssh
import qualified Propellor.Property.Postfix as Postfix
import qualified Propellor.Property.Gpg as Gpg
import qualified Propellor.Property.Sudo as Sudo

server :: [Host] -> Property (HasInfo + DebianLike)
server hosts = propertyList "branchable server" $ props
	& "/etc/timezone" `File.hasContent` ["Etc/UTC"]
	& "/etc/locale.gen" `File.containsLines`
		[ "en_GB.UTF-8 UTF-8"
		, "en_US.UTF-8 UTF-8"
		, "fi_FI.UTF-8 UTF-8"
		]
		`onChange` (cmdProperty "locale-gen" [] `assume` MadeChange)

	& Apt.installed ["etckeeper", "ssh", "popularity-contest"]
	& Apt.serviceInstalledRunning "apache2"
	& Apt.serviceInstalledRunning "ntp"

	& Apt.serviceInstalledRunning "openssh-server"
	& Ssh.passwordAuthentication False
	& Ssh.hostKeys (Context "branchable.com")
		[ (SshDsa, "ssh-dss AAAAB3NzaC1kc3MAAACBAK9HnfpyIm8aEhKuF5oz6KyaLwFs2oWeToVkqVuykyy5Y8jWDZPtkpv+1TeOnjcOvJSZ1cCqB8iXlsP9Dr5z98w5MfzsRQM2wIw0n+wvmpPmUhjVdGh+wTpfP9bcyFHhj/f1Ymdq9hEWB26bnf4pbTbJW2ip8ULshMvn5CQ/ugV3AAAAFQCAjpRd1fquRiIuLJMwej0VcyoZKQAAAIBe91Grvz/icL3nlqXYrifXyr9dsw8bPN+BMu+hQtFsQXNJBylxwf8FtbRlmvZXmRjdVYqFVyxSsrL2pMsWlds51iXOr9pdsPG5a4OgJyRHsveBz3tz6HgYYPcr3Oxp7C6G6wrzwsaGK862SgRp/bbD226k9dODRBy3ogMhk/MvAgAAAIEApfknql3vZbDVa88ZnwbNKDOv8L1hb6blbKAMt2vJbqJMvu3EP9CsP9hGyEQh5YCAl2F9KEU3bJXN1BG76b7CiYtWK95lpL1XmCCWnJBCcdEhw998GfJS424frPw7qGmXLxJKYxEyioB90/IDp2dC+WaLcLOYHM9SroCQTIK5A1g= root@pell")
		, (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEA1M0aNLgcgcgf0tkmt/8vCDZLok8Xixz7Nun9wB6NqVXxfzAR4te+zyO7FucVwyTY5QHmiwwpmyNfaC21AAILhXGm12SUKSAirF9BkQk7bhQuz4T/dPlEt3d3SxQ3OZlXtPp4LzXWOyS0OXSzIb+HeaDA+hFXlQnp/gE7RyAzR1+xhWPO7Mz1q5O/+4dXANnW32t6P7Puob6NsglVDpLrMRYjkO+0RgCVbYMzB5+UnkthkZsIINaYwsNhW2GKMKbRZeyp5en5t1NJprGXdw0BqdBqd/rcBpOxmhHE1U7rw+GS1uZwCFWWv0aZbaXEJ6wY7mETFkqs0QXi5jtoKn95Gw== root@pell")
		]

	& Apt.installed ["procmail", "bsd-mailx"]
	& "/etc/aliases" `File.hasPrivContentExposed` (Context "branchable.com")
		`onChange` Postfix.newaliases
	& "/etc/mailname" `File.hasContent` ["branchable.com"]
	& Postfix.installed
	& Postfix.mainCf ("mailbox_command", "procmail -a \"$EXTENSION\"")
	
	-- Obnam is run by a cron job in ikiwiki-hosting.
	& "/etc/obnam.conf" `File.hasContent`
		[ "[config]"
		, "repository = sftp://joey@eubackup.kitenet.net/home/joey/lib/backup/pell.obnam"
		, "log = /var/log/obnam.log"
		, "encrypt-with = " ++ obnamkey
		, "log-level = info"
		, "log-max = 1048576"
		, "keep = 7d,5w,12m"
		, "upload-queue-size = 128"
		, "lru-size = 128"
		]
	& Gpg.keyImported (Gpg.GpgKeyId obnamkey) (User "root")
	& Ssh.userKeys (User "root") (Context "branchable.com")
		[ (SshRsa, "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC2PqTSupwncqeffNwZQXacdEWp7L+TxllIxH7WjfRMb3U74mQxWI0lwqLVW6Fox430DvhSqF1y5rJBvTHh4i49Tc9lZ7mwAxA6jNOP6bmdfteaKKYmUw5qwtJW0vISBFu28qBO11Nq3uJ1D3Oj6N+b3mM/0D3Y3NoGgF8+2dLdi81u9+l6AQ5Jsnozi2Ni/Osx2oVGZa+IQDO6gX8VEP4OrcJFNJe8qdnvItcGwoivhjbIfzaqNNvswKgGzhYLOAS5KT8HsjvIpYHWkyQ5QUX7W/lqGSbjP+6B8C3tkvm8VLXbmaD+aSkyCaYbuoXC2BoJdS7Jh8phKMwPJmdYVepn")
		]
	& Ssh.knownHost hosts "eubackup.kitenet.net" (User "root")
	& Ssh.knownHost hosts "usw-s002.rsync.net" (User "root")

	& adminuser "joey"
	& adminuser "liw"
  where
	obnamkey = "41E1A9B9"
	adminuser u = propertyList ("admin user " ++ u) $ props
		& User.accountFor (User u)
		& User.hasSomePassword (User u)
		& Sudo.enabledFor (User u)
		& User.hasGroup (User u) (Group "adm")
		& User.hasGroup (User u) (Group "systemd-journal")
