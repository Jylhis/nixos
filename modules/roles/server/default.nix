{
  config,
  lib,
  ...
}:
let
  cfg = config.jylhis.role.server;
in
{

  options.jylhis.role.server = {
    enable = lib.mkEnableOption "This systems is used as server";
  };

  config = lib.mkIf cfg.enable {
    boot.growPartition = true;

    environment = {
      # Print the URL instead on servers
      variables.BROWSER = "echo";
      # Don't install the /lib/ld-linux.so.2 and /lib64/ld-linux-x86-64.so.2
      # stubs. Server users should know what they are doing.
      stub-ld.enable = lib.mkDefault false;
    };

    # Restrict the number of boot entries to prevent full /boot partition.
    #
    # Servers don't need too many generations.
    boot.loader.grub.configurationLimit = lib.mkDefault 5;
    boot.loader.systemd-boot.configurationLimit = lib.mkDefault 5;

    programs.command-not-found.enable = lib.mkDefault false;

    # Make sure firewall is enabled
    networking.firewall.enable = true;

    # If the user is in @wheel they are trusted by default.
    nix.settings.trusted-users = [ "@wheel" ];
    security.sudo.wheelNeedsPassword = false;

    # Enable SSH everywhere
    services.openssh = {
      enable = true;
      allowSFTP = lib.mkDefault false;
      settings = {
        PasswordAuthentication = lib.mkDefault false;
        X11Forwarding = lib.mkDefault false;
        PermitRootLogin = lib.mkDefault "prohibit-password";
      };
    };

    services.fail2ban = {
      bantime-increment = {
        enable = lib.mkDefault true;
        formula = lib.mkDefault "ban.Time * math.exp(float(ban.Count+1)*banFactor)/math.exp(1*banFactor)";
        maxtime = lib.mkDefault "168h"; # Do not ban for more than 1 week
      };
    };

    # UTC everywhere!
    time.timeZone = lib.mkDefault "UTC";

    # No mutable users by default
    users.mutableUsers = false;

    # Given that our systems are headless, emergency mode is useless.
    # We prefer the system to attempt to continue booting so
    # that we can hopefully still access it remotely.
    boot.initrd.systemd.suppressedUnits = lib.mkIf config.systemd.enableEmergencyMode [
      "emergency.service"
      "emergency.target"
    ];

    systemd = {
      # Given that our systems are headless, emergency mode is useless.
      # We prefer the system to attempt to continue booting so
      # that we can hopefully still access it remotely.
      enableEmergencyMode = false;

      # For more detail, see:
      #   https://0pointer.de/blog/projects/watchdog.html
      watchdog = {
        # systemd will send a signal to the hardware watchdog at half
        # the interval defined here, so every 7.5s.
        # If the hardware watchdog does not get a signal for 15s,
        # it will forcefully reboot the system.
        runtimeTime = lib.mkDefault "15s";
        # Forcefully reboot if the final stage of the reboot
        # hangs without progress for more than 30s.
        # For more info, see:
        #   https://utcc.utoronto.ca/~cks/space/blog/linux/SystemdShutdownWatchdog
        rebootTime = lib.mkDefault "30s";
        # Forcefully reboot when a host hangs after kexec.
        # This may be the case when the firmware does not support kexec.
        kexecTime = lib.mkDefault "1m";
      };

      sleep.extraConfig = ''
        AllowSuspend=no
        AllowHibernation=no
      '';
    };

    # freedesktop xdg files
    xdg.autostart.enable = lib.mkDefault false;
    xdg.icons.enable = lib.mkDefault false;
    xdg.menus.enable = lib.mkDefault false;
    xdg.mime.enable = lib.mkDefault false;
    xdg.sounds.enable = lib.mkDefault false;

    # De-duplicate store paths using hardlinks except in containers
    # where the store is host-managed.
    nix.optimise.automatic = lib.mkDefault (!config.boot.isContainer);

    # Only allow members of the wheel group to execute sudo by setting the executable’s permissions accordingly. This prevents users that are not members of wheel from exploiting vulnerabilities in sudo such as CVE-2021-3156.
    security.sudo.execWheelOnly = true;
    # Don't lecture the user. Less mutable state.
    security.sudo.extraConfig = ''
      Defaults lecture = never
    '';

    # Make sure the serial console is visible in qemu when testing the server configuration
    # with nixos-rebuild build-vm
    virtualisation.vmVariant.virtualisation.graphics = lib.mkDefault false;
  };

}
