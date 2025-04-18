# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  lib,
  self,
  pkgs,
  unstable,
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
      intel-vaapi-driver
      vaapiVdpau
      intel-compute-runtime # OpenCL filter support (hardware tonemapping and subtitle burn-in)
      #intel-compute-runtime-legacy1
      #vpl-gpu-rt # QSV on 11th gen or newer
      intel-media-sdk # QSV up to 11th gen
    ];
  };

  nix = {

    sshServe = {
      enable = true;
      keys =
        config.users.users.nixremote.openssh.authorizedKeys.keys
        ++ config.users.users.markus.openssh.authorizedKeys.keys;
    };
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
        config.users.users.nixremote.name
      ];
    };
  };

  users = {

    groups = {
      media-srv = {
        members = [ "markus" ];

      };
      nixremote = { };
    };

    users = {
      nixremote = {
        isSystemUser = true;
        useDefaultShell = true;
        group = "nixremote";
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIG1FlfEoerMLkY/a1/cIx57dlg6gbeqpsxbzHB8V9X6K root@macbook-air"
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAzxssZrZsAZ2oeIXNW7dMxwGQt7ZFOuqT0Oyc1o/l4V root@mac-mini"
        ];
      };
      media-srv = {
        group = "media-srv";
        isSystemUser = true;
        home = "/var/lib/media-srv";
        createHome = true;
        description = "Shared user for media server stuff";
      };
      root.initialPassword = "root"; # FIXME
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
    firewall = {
      enable = true;
      allowedTCPPorts = [ config.services.grafana.settings.server.http_port ];
    };
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

  users.users.immich.extraGroups = [
    "video"
    "render"
  ];

  # Disable default sync folder syncthing
  systemd.services.syncthing.environment.STNODEFAULTFOLDER = "true"; # Don't create default ~/Sync folder
  services = {
    samba = {
      enable = true;
      nmbd.enable = false;
      settings = {
        global = {
          "workgroup" = "WORKGROUP";
          "server string" = "smbnix";
          "netbios name" = "smbnix";
          "hosts allow" = "100. 192.168.0. localhost 127.0.0.1";
          "hosts deny" = "0.0.0.0/0";
        };
        "homes" = {
          "path" = "/data/samba/%S";
          "read only" = "no";
        };
        "data" = {
          "path" = "/data";
          "browseable" = "yes";
          "read only" = "no";
          "guest ok" = "no";
          "valid users" = config.users.users.markus.name;
          "create mask" = "0644";
          "directory mask" = "0755";
          "inherit permissions" = "yes";
          #      "force user" = config.users.users.markus.name;
          #      "force group" = config.users.users.markus.group;
        };
      };
    };
    hydra = {
      enable = false;
      port = 4000;
      #  maxServers = 1;
      #  minSpareServers = 0;
      # hydraURL = "http://localhost:3000"; # externally visible URL
      #notificationSender = "hydra@localhost"; # e-mail of Hydra service
      # a standalone Hydra will require you to unset the buildMachinesFiles list to avoid using a nonexistant /etc/nix/machines
      buildMachinesFiles = [ ];
      # you will probably also want, otherwise *everything* will be built from scratch
      useSubstitutes = true;
    };

    immich = {
      enable = true;
      #openFirewall = true;
      host = "0.0.0.0";
      settings = builtins.fromJSON (builtins.readFile ./immich.json);
      # `null` will give access to all devices.
      # You may want to restrict this by using something like `[ "/dev/dri/renderD128" ]`
      accelerationDevices = null;
      mediaLocation = "/data/Photos";
    };

    rclone-sync = {
      enable = false;
      source = "OneDrive:/";
      destination = "/data/personal/OneDrive/";
      extraArgs = [
        "--dry-run"
        "--no-update-modtime"
        "--backup-dir=/data/personal/rclone-backup"
      ];
    };

    fail2ban = {
      enable = true;
      maxretry = 5;
      bantime = "15m";
      bantime-increment = {
        enable = true;
        formula = "ban.Time * math.exp(float(ban.Count+1)*banFactor)/math.exp(1*banFactor)";
        factor = "4";
        maxtime = "168h"; # Do not ban for more than 1 week
      };
      ignoreIP = [
        "100.64.0.0/10"
        "127.0.0.1/8"
      ];
    };
    zfs.autoScrub.enable = true;
    openssh = {
      enable = true;
      allowSFTP = true;
      settings = {
        PasswordAuthentication = false;
        PermitRootLogin = "prohibit-password";
        X11Forwarding = false;
      };

    };
    tailscale = {
      enable = true;
      openFirewall = true;
      authKeyFile = config.sops.secrets.tailscale_auth_key.path;
    };

    sonarr = {
      enable = true; # port: 8989
      user = "media-srv";
      group = "media-srv";

    };
    radarr = {
      enable = true; # port: 7878
      user = "media-srv";
      group = "media-srv";
    };
    lidarr = {
      user = "media-srv";
      group = "media-srv";
      enable = true; # port: 8686
    };
    bazarr = {
      user = "media-srv";
      group = "media-srv";
      enable = true; # port: 6767
    };
    prowlarr = {

      enable = true; # port: 9696
    };
    readarr = {
      user = "media-srv";
      group = "media-srv";
      enable = true; # port: 8787

    };
    jellyfin = {
      enable = true; # port: https: 8920 & http: 8096
      user = "media-srv";
      group = "media-srv";
    };

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      user = "media-srv";
      group = "media-srv";
      guiAddress = "0.0.0.0:8384";
      settings = {
        options = {
          urAccepted = -1;

        };
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
      scrapeConfigs = [
        {
          job_name = "main";
          static_configs = [
            {
              targets = [
                "localhost:${toString config.services.prometheus.exporters.node.port}"
                "localhost:${toString config.services.prometheus.exporters.collectd.port}"
                "localhost:${toString config.services.prometheus.exporters.process.port}"
                "localhost:${toString config.services.prometheus.exporters.systemd.port}"
                "localhost:${toString config.services.prometheus.exporters.zfs.port}"

              ];
            }
          ];
        }
        {
          job_name = "exportarr";
          static_configs = [
            {
              targets = [
                "localhost:${toString config.services.prometheus.exporters.exportarr-radarr.port}"
                "localhost:${toString config.services.prometheus.exporters.exportarr-sonarr.port}"
                "localhost:${toString config.services.prometheus.exporters.exportarr-bazarr.port}"
                "localhost:${toString config.services.prometheus.exporters.exportarr-lidarr.port}"
                "localhost:${toString config.services.prometheus.exporters.exportarr-readarr.port}"
                "localhost:${toString config.services.prometheus.exporters.exportarr-prowlarr.port}"
              ];
            }
          ];
        }

      ];
      exporters = {
        exportarr-bazarr = {
          port = 9708;
          url = "http://localhost:6767";
          enable = true;
          apiKeyFile = config.sops.secrets.bazarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-lidarr = {
          port = 9709;
          url = "http://localhost:8686";
          enable = true;
          apiKeyFile = config.sops.secrets.lidarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-prowlarr = {
          port = 9710;
          url = "http://localhost:9696";
          enable = true;
          apiKeyFile = config.sops.secrets.prowlarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-radarr = {
          port = 9711;
          url = "http://localhost:7878";
          enable = true;
          apiKeyFile = config.sops.secrets.radarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-readarr = {
          port = 9712;
          url = "http://localhost:8787";
          enable = true;
          apiKeyFile = config.sops.secrets.readarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-sonarr = {
          port = 9713;
          url = "http://localhost:8989";
          enable = true;
          apiKeyFile = config.sops.secrets.sonarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        zfs.enable = true;

        node = {

          enable = true;
          enabledCollectors = [
            "logind"
            "network_route"
            "perf"
            #	    "processes"
            #	    "systemd"
          ];
          disabledCollectors = [
            "btrfs"
            "infiniband"
            "nfs"
            "nfsd"
            "selinux"
          ];
        };
        process = {
          enable = true;
          settings.process_names = [
            # Remove nix store path from process name
            {
              name = "{{.Matches.Wrapped}} {{ .Matches.Args }}";
              cmdline = [ "^/nix/store[^ ]*/(?P<Wrapped>[^ /]*) (?P<Args>.*)" ];
            }
          ];

        };

        # https://github.com/prometheus-community/systemd_exporter?tab=readme-ov-file#configuration
        systemd.enable = true;
      };
    };

    loki = {
      enable = true;
      configFile = ./loki-config.yaml;
      extraFlags = [
        "-reporting.enabled=0"
      ];
    };

    alloy = {
      enable = true;
      extraFlags = [ "--disable-reporting" ];
    };
    # Port: 3000
    grafana = {
      enable = true;
      declarativePlugins = [
        self.packages.x86_64-linux.grafana-treemap-panel
      ];
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

      provision = {
        enable = true;
        datasources = {
          settings.datasources = [
            {
              name = "Prometheus";
              type = "prometheus";
              url = "http://${config.services.prometheus.listenAddress}:${toString config.services.prometheus.port}";
            }
            {
              name = "Loki";
              type = "loki";
              url = "http://localhost:3100";
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

      sonarr_api_key = {
        mode = "0444";
      };

      bazarr_api_key = {
        mode = "0444";
      };

      lidarr_api_key = {
        mode = "0444";
      };

      radarr_api_key = {
        mode = "0444";
      };

      readarr_api_key = {
        mode = "0444";
      };

      prowlarr_api_key = {
        mode = "0444";
      };

    };
  };

  environment = {
    etc = {
      "alloy/config.alloy".source = ./config.alloy;
    };
    systemPackages = with pkgs; [
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
      rclone
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
    packageOverrides = pkgs: {
      ffmpeg-full = unstable.ffmpeg-full.override {
        withUnfree = true;
        withOpengl = true;
      };

      bazarr = unstable.bazarr.override {
        ffmpeg = pkgs.ffmpeg-full;
      };

      sonarr = unstable.sonarr.override {
        ffmpeg = pkgs.ffmpeg-full;
        withFFmpeg = true;
      };
      inherit (unstable) jellyfin;
      inherit (unstable) lidarr;
      inherit (unstable) radarr;
      inherit (unstable) readarr;
      inherit (unstable) prowlarr;
    };

    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
      "aspnetcore-runtime-6.0.36"
    ];

    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };

  };

  system.stateVersion = "24.11";
}
