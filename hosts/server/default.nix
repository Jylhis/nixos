# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  pkgs,
  config,
  ...
}:
let
  zfsCompatibleKernelPackages = lib.filterAttrs (
    name: kernelPackages:
    (builtins.match "linux_[0-9]+_[0-9]+" name) != null
    && (builtins.tryEval kernelPackages).success
    && (!kernelPackages.${config.boot.zfs.package.kernelModuleAttribute}.meta.broken)
  ) pkgs.linuxKernel.packages;
  latestKernelPackage = lib.last (
    lib.sort (a: b: (lib.versionOlder a.kernel.version b.kernel.version)) (
      builtins.attrValues zfsCompatibleKernelPackages
    )
  );
in
{
  imports = [
    ./disko-config.nix
    ./hardware-configuration.nix
    ./networking-details.nix
  ];

  # TODO: Hetzner serial access
  disko.devices.disk.main.device = "/dev/sda";
  disko.devices.disk.secondary.device = "/dev/sdb";

  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver
      #intel-vaapi-driver
      vaapiVdpau
      intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
      #vpl-gpu-rt # QSV on 11th gen or newer
      intel-media-sdk # QSV up to 11th gen
    ];
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

  users = {

    groups = {
      prv-media = {
        members = [
          config.users.users.syncthing.name
          config.users.users.jellyfin.name
	  config.users.users.sonarr.name
        ];
      };
    };

    users = {

      root.password = "root"; # FIXME
      root.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIK4zXcaYT+RTxvAjUjE3B33kwwxCOo4ApI4diLnajbUT"
      ];
    };
  };

  # Bootloader.
  boot = {
    kernelParams = [
      "numa=off"
      "mitigations=off"
      "nosgx"
    ];
    supportedFilesystems = [ "zfs" ];
    zfs = {

      devNodes = "/dev";
      forceImportRoot = false;
      forceImportAll = false;
      passwordTimeout = 60;
      requestEncryptionCredentials = false;
    };
    kernelPackages = latestKernelPackage;
    loader = {
      #efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
        zfsSupport = true;
        efiSupport = true;
        efiInstallAsRemovable = true;
        mirroredBoots = [
          # FIXME
          {
            devices = [ "nodev" ];
            path = "/boot";
          }
        ];
        device = "nodev";
      };
    };

    initrd = {
      systemd.enable = true;
      network = {
        # This will use udhcp to get an ip address.
        # Make sure you have added the kernel module for your network driver to `boot.initrd.availableKernelModules`,
        # so your initrd can load it!
        # Static ip addresses might be configured using the ip argument in kernel command line:
        # https://www.kernel.org/doc/Documentation/filesystems/nfs/nfsroot.txt
        enable = true;
        ssh = {
          enable = false; # TODO
          # To prevent ssh clients from freaking out because a different host key is used,
          # a different port for ssh is useful (assuming the same host has also a regular sshd running)
          port = 2222;
          # hostKeys paths must be unquoted strings, otherwise you'll run into issues with boot.initrd.secrets
          # the keys are copied to initrd from the path specified; multiple keys can be set
          # you can generate any number of host keys using
          # `ssh-keygen -t ed25519 -N "" -f /path/to/ssh_host_ed25519_key`
          #          hostKeys = [ /path/to/ssh_host_rsa_key ];
          # public ssh key used for login
          authorizedKeys = [
            "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEPS1QmyH3mqvHXANosiq2h/TCacV9nXFsbWBtEC45GW"
          ];
        };
      };
    };

  };

  networking = {
    hostId = "91312b0a";
    hostName = "server";
    firewall.allowedTCPPorts = [ config.services.grafana.settings.server.http_port ];
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

  # Disable default sync folder syncthing
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true"; # Don't create default ~/Sync folder
  services = {
    fail2ban = {
      enable = true;
    };
    zfs.autoScrub.enable = true;
    openssh = {
      enable = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "prohibit-password";
      };

    };
    tailscale = {
      enable = true;
      openFirewall = true;
      authKeyFile = config.sops.secrets.tailscale_auth_key.path;
    };

    sonarr.enable = true; # port: 8989
    radarr.enable = false; # port: 7878
    lidarr.enable = false; # port: 8686
    bazarr.enable = false; # port: 6767
    prowlarr.enable = true; # port: 9696
    readarr.enable = false; # port: 8787
    jellyfin = {
      enable = true; # port: https: 8920 & http: 8096
      openFirewall = true;
    };

    syncthing = {
      enable = true;
      openDefaultPorts = true;

      guiAddress = "0.0.0.0:8384";
      settings = {
        gui = {
          user = "myuser";
          password = "mypassword";
        };
        devices = import ./syncthing-devices.nix;

        folders = import ./syncthing-folders.nix { inherit config; };
      };
    };

    # Monitoring

    # port: 9090
    prometheus = {
      enable = true;
      globalConfig.scrape_interval = "10s"; # "1m"
      scrapeConfigs = [
        {
          job_name = "node";
          static_configs = [
            {
              targets = [
                "localhost:${toString config.services.prometheus.exporters.node.port}"
                "localhost:${toString config.services.prometheus.exporters.collectd.port}"
                "localhost:${toString config.services.prometheus.exporters.process.port}"
                "localhost:${toString config.services.prometheus.exporters.systemd.port}"
                "localhost:${toString config.services.prometheus.exporters.zfs.port}"
                #"localhost:${toString config.services.prometheus.exporters.exportarr-radarr.port}"
                #"localhost:${toString config.services.prometheus.exporters.exportarr-sonarr.port}"
                "100.100.100.100:80" # tailscale
              ];
            }
          ];
        }
      ];
      exporters = {
        collectd.enable = true;
        # TODO: For exportarr: api key file with sops
        #   exportarr-bazarr.enable = true;
        #   exportarr-lidarr.enable = true;
        #   exportarr-prowlarr.enable = true;
        #exportarr-radarr.enable = true;
        #   exportarr-readarr.enable = true;
        #exportarr-sonarr.enable = true;
        zfs.enable = true;
        node.enable = true;
        #   ping.enable = true;
        process.enable = true;
        systemd.enable = true;

      };
    };

    # Port: 3000
    grafana = {
      enable = true;
      settings = {
        analytics = {
          reporting_enabled = false;
          feedback_links_enabled = false;
          check_for_updates = false;
          check_for_plugin_updates = false;

        };
        server = {
          enforce_domain = false;
          enable_gzip = true;
          http_addr = "0.0.0.0";
        };
        security = {
          disable_gravatar = true;
          admin_user = "admin";
          admin_password = "$__file{${config.sops.secrets.grafana_admin_password.path}}";
        };
      };

      # TODO: Import
      # https://grafana.com/grafana/dashboards/1860-node-exporter-full/
      # https://grafana.com/grafana/dashboards/12530-sonarr-v3/
      # https://grafana.com/grafana/dashboards/12896-radarr-v3/
      provision = {
        enable = true;
        datasources = {
          settings.datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            }
          ];
        };
      };
    };

    # Experimental
    #shiori.enable = true;
    #outline.enable = true;
    #your_spotify.enable = true;

  };

  sops = {
    age = {
      generateKey = true;
    };
    defaultSopsFile = ../../secrets/default.yaml;
    secrets = {
      tailscale_auth_key = { };
      grafana_admin_password = {
        owner = config.systemd.services.grafana.serviceConfig.User;
      };
    };
  };

  environment = {
    systemPackages = with pkgs; [
      tailscale
      vim
      sops
      age
    ];
  };

  users.groups.wheel.members = [
    config.users.users.markus.name
  ];
  security = {
    sudo.wheelNeedsPassword = false;
    tpm2 = {
      enable = true;
      pkcs11.enable = true;
      tctiEnvironment.enable = true;
    };
  };

  # Allow unfree packages
  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
      "aspnetcore-runtime-6.0.36"
    ];
    # TODO
    # packageOverrides = pkgs: {
    #   vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };
    # };
  };

  system.stateVersion = "24.11";
}
