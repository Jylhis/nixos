# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  #  self,
  # lib,
  pkgs,
  config,
  sops,
  ...
}:
{
  imports = [
    ./hardware-configuration.nix
  ];

  sops = {
    age.generateKey = true;
    defaultSopsFile = ../../secrets/server.yaml;
    # This will automatically import SSH keys as age keys
  #sops.age.sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  # This is using an age key that is expected to already be in the filesystem
  #sops.age.keyFile = "/var/lib/sops-nix/key.txt";
  # This will generate a new key if the key specified above does not exist

  # This is the actual specification of the secrets.
  #sops.secrets.example-key = {};
  #sops.secrets."myservice/my_subdir/my_secret" = {};
  };

  nix = {
    gc = {
      automatic = true;
      options = "--delete-older-than 14d";
      randomizedDelaySec = "14m";
    };
    optimise.automatic = true;

    settings = {
      accept-flake-config = true;
      auto-optimise-store = true;
      keep-outputs = true;
      experimental-features = [
        "nix-command"
        "flakes"
      ];
      trusted-users = [
        config.users.users.root.name
      ];
    };
  };

  users.users.root.initialPassword = "root";

  # Bootloader.
  boot = {
    loader = {
      grub = {

        enable = true;
        device = "/dev/sda";
        useOSProber = true;
      };
    };

    initrd.systemd.enable = true;

  };

  networking = {
    firewall = {
      enable = false;
      allowPing = true;
    };
    hostName = "server";

  };

  time.timeZone = "Europe/Zurich";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "de_CH.UTF-8";
      LC_IDENTIFICATION = "de_CH.UTF-8";
      LC_MEASUREMENT = "de_CH.UTF-8";
      LC_MONETARY = "de_CH.UTF-8";
      LC_NAME = "de_CH.UTF-8";
      LC_NUMERIC = "de_CH.UTF-8";
      LC_PAPER = "de_CH.UTF-8";
      LC_TELEPHONE = "de_CH.UTF-8";
      LC_TIME = "de_CH.UTF-8";
    };
  };

  console.useXkbConfig = true;

  services = {
    openssh.enable = true;
    tailscale = {
      enable = true;
      useRoutingFeatures = "server";
      disableTaildrop = true;
    };

    sonarr.enable = true; # port: 8989
    radarr.enable = true; # port: 7878
    lidarr.enable = true; # port: 8686
    bazarr.enable = true; # port: 6767
    prowlarr.enable = true; # port: 9696
    readarr.enable = true; # port: 8787
    jellyfin.enable = true; # port: https: 8920 & http: 8096

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      settings.gui = {
        user = "myuser";
        password = "mypassword";
      };
    };

    # Monitoring

    # port: 9090
    prometheus = {
      enable = true;
      exporters = {
        collectd.enable = true;
        #   exportarr-bazarr.enable = true;
        #   exportarr-lidarr.enable = true;
        #   exportarr-prowlarr.enable = true;
        #   exportarr-radarr.enable = true;
        #   exportarr-readarr.enable = true;
        #   exportarr-sonarr.enable = true;
        node.enable = true;
        #   ping.enable = true;
        process.enable = true;
        systemd.enable = true;
        #   zfs.enable = true;
        #   wireguard.enable = true;
      };
    };

    # port: 19999
    netdata =
      let
        package = pkgs.netdata.override {
          withCloud = true;
          withCloudUi = true;
          withConnPrometheus = true;
          withConnPubSub = true;
        };
      in
      {
        inherit package;
        enable = true;
        python.enable = true;
        python.recommendedPythonPackages = true;
      };
    # Experimental
    #shiori.enable = true;
    #outline.enable = true;
    #your_spotify.enable = true;
  };

  environment = {
    systemPackages = with pkgs; [
    ];
  };

  # Allow unfree packages
  nixpkgs.config = {
    allowUnfree = true;
    warnUndeclaredOptions = true;
    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
      "aspnetcore-runtime-6.0.36"
    ];
  };

  system.stateVersion = "24.11";
}
