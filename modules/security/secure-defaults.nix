# Sources:
# https://xeiaso.net/blog/paranoid-nixos-2021-07-18/
# TODO:
# https://github.com/cynicsketch/nix-mineral
# https://www.debian.org/doc/manuals/securing-debian-manual/index.en.html
# https://madaidans-insecurities.github.io/guides/linux-hardening.html
# https://privsec.dev/posts/linux/desktop-linux-hardening/
# https://github.com/Kicksecure/security-misc
# https://github.com/secureblue/secureblue
# https://wiki.nixos.org/wiki/Systemd/Hardening

_: {
  # TODO: systemd-analyze security yourservicename.service

  config = {
    networking.firewall.enable = true;
    nix.allowedUsers = [ "@wheel" ];
    security = {
      sudo.execWheelOnly = true;

      # TODO: useful? https://www.debian.org/doc/manuals/securing-debian-manual/log-alerts.en.html
      auditd.enable = true;
      audit.enable = true;
    };
    # TODO: Kernel patches https://www.debian.org/doc/manuals/securing-debian-manual/kernel-patches.en.html
    # TODO: Quota https://www.debian.org/doc/manuals/securing-debian-manual/ch04s17.en.html
    # TODO: https://www.debian.org/doc/manuals/securing-debian-manual/restrict-console-login.en.html
    # TODO: https://www.debian.org/doc/manuals/securing-debian-manual/restrict-sysrq.en.html
    # TODO: PAM https://www.debian.org/doc/manuals/securing-debian-manual/ch04s11.en.html
    services = {
      openssh = {
        # TODO: https://www.debian.org/doc/manuals/securing-debian-manual/sec-services.en.html
        passwordAuthentication = false;
        allowSFTP = false; # Don't set this if you need sftp
        challengeResponseAuthentication = false;
        X11Forwarding = false;
        extraConfig = ''
          AllowTcpForwarding yes
          AllowAgentForwarding no
          AllowStreamLocalForwarding no
          AuthenticationMethods publickey
        '';
      };
    };
  };
}
