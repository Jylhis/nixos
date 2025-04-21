{
  lib,
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./disko-config.nix
    ./hardware-configuration.nix
    ./networking-details.nix
    ./private.nix
    ../../modules/roles/server
  ];

  jylhis = {
    role.server.enable = true;
  };

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
        "@${config.users.groups.wheel.name}"
      ];
    };
  };

  users = {
    groups = {
      wheel.members = [
        config.users.users.markus.name
      ];
      media-srv = {
        members = [
          config.users.users.markus.name
          config.users.users.sonarr.name
          config.users.users.radarr.name
          config.users.users.bazarr.name
          config.users.users.lidarr.name
          #config.users.users.prowlarr.name
          config.users.users.readarr.name
          config.users.users.jellyfin.name
          config.users.users.syncthing.name
        ];

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

    loader = {
      #efi.canTouchEfiVariables = true;
      grub = {
        enable = true;
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
      };
    };

  };

  networking = {
    hostId = "91312b0a";
    hostName = "server";
  };

  time.timeZone = "Europe/Zurich";

  # Select internationalization properties.
  i18n = {
    defaultLocale = "en_US.UTF-8";
  };

  users.users.immich.extraGroups = [
    "video"
    "render"
  ];

  services = {
    opensearch-dashboards = {
      enable = false;
      listenAddress = "0.0.0.0";
    };

    paperless = {
      enable = true;
      address = "0.0.0.0";
      consumptionDirIsPublic = true;
      settings = {
        PAPERLESS_CONSUMER_IGNORE_PATTERN = [
          ".DS_STORE/*"
          "desktop.ini"
        ];
        PAPERLESS_OCR_LANGUAGE = "fin+eng+fra+deu";
      };
    };

    # NOTES
    # livebook
    # daliytxt?

    # BUDGET
    # firefly-iii
    # actual

    # TOOLS
    # cyberchef
    # https://string.is/
    # https://github.com/yourselfhosted/slash

    # MEDIA
    # watcharr
    # komga
    # kavita
    # atsumeru
    # mylar3
    # LazyLibrarian
    # bookbounty
    # bitmagnet

    # BOOKMARK
    # shiori
    # karakeep

    fluent-bit = {
      enable = false;
      settings = {
        pipeline = {
          inputs = [
            {
              name = "systemd";
              tag = "host.*";
              #systemd_filter = "_SYSTEMD_UNIT=sonarr.service";
            }
            # { FIXME: Permission denied
            #   name = "kmsg";
            #   tag = "kernel";
            # }
            # {
            #   name = "node_exporter_metrics";
            #   tag = "node_metrics";
            #   scrape_interval = 2;
            # }
            # {
            #   name = "process_exporter_metrics";
            #   tag = "process_metrics";
            #   scrape_interval = 2;
            # }

          ];
          outputs = [
            {
              name = "opensearch";
              match = "*";
              host = "127.0.0.1";
              index = "fluent-bit";
              suppress_type_name = true;
              port = 9200;
            }
          ];
        };
        service = {
          grace = 30;
        };
      };
    };
    opensearch = {
      enable = false;
      package = pkgs.opensearch.overrideAttrs (old: {
        # Workaround for packaging bug (deleting opensearch-cli breaks opensearch-plugin command)
        installPhase =
          builtins.replaceStrings [ "rm $out/bin/opensearch-cli\n" "--replace" ] [ "" "--replace-fail" ]
            old.installPhase;

        # postInstall = ''
        #   # Install some plugins
        #   $out/bin/opensearch-plugin install analysis-icu analysis-smartcn analysis-kuromoji analysis-stempel
        # '';
      });
      settings = {
        "network.host" = "0.0.0.0";
        "plugins.query.datasources.encryption.masterkey" = "2ccc9d70a449ace1a8858604"; # FIXME
      };
    };
    suricata = {
      enable = false;
      settings = {
        af-packet = [
          {
            inherit (config.networking.defaultGateway) interface;
            cluster-id = 99;
            cluster-type = "cluster_flow";
            defrag = "yes";
          }
        ];
      };
    };
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
        };
      };
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

    fail2ban = {
      enable = true;
      ignoreIP = [
        "100.64.0.0/10"
        "127.0.0.1/8"
      ];
    };

    openssh = {
      enable = true;
    };

    tailscale = {
      enable = true;
      openFirewall = true;
      authKeyFile = config.sops.secrets.tailscale_auth_key.path;
    };

    recyclarr = {
      # TODO
      enable = false;
    };
    sonarr = {
      enable = true; # port: 8989
    };
    radarr = {
      enable = true; # port: 7878
    };
    lidarr = {
      enable = true; # port: 8686
    };
    bazarr = {
      enable = true; # port: 6767
    };
    prowlarr = {
      enable = true; # port: 9696
    };
    readarr = {
      enable = true; # port: 8787

    };
    jellyfin = {
      enable = true; # port: https: 8920 & http: 8096
    };

    syncthing = {
      enable = true;
      openDefaultPorts = true;
      guiAddress = "0.0.0.0:8384";
      settings = {
        options = {
          urAccepted = -1;
        };
      };
    };

    # port: 9090
    prometheus = {
      enable = false;
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
          enable = true;
          port = 9708;
          url = "http://localhost:6767";
          apiKeyFile = config.sops.secrets.bazarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-lidarr = {
          enable = true;
          port = 9709;
          url = "http://localhost:8686";
          apiKeyFile = config.sops.secrets.lidarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-prowlarr = {
          enable = true;
          port = 9710;
          url = "http://localhost:9696";
          apiKeyFile = config.sops.secrets.prowlarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-radarr = {
          enable = true;
          port = 9711;
          url = "http://localhost:7878";
          apiKeyFile = config.sops.secrets.radarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-readarr = {
          enable = true;
          port = 9712;
          url = "http://localhost:8787";
          apiKeyFile = config.sops.secrets.readarr_api_key.path;
          environment = {
            ENABLE_ADDITIONAL_METRICS = "true";
            LOG_LEVEL = "error";
          };
        };

        exportarr-sonarr = {
          enable = true;
          port = 9713;
          url = "http://localhost:8989";
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
            {
              # Remove nix store path from process name
              name = "{{.Matches.Wrapped}} {{ .Matches.Args }}";
              cmdline = [ "^/nix/store[^ ]*/(?P<Wrapped>[^ /]*) (?P<Args>.*)" ];
            }
          ];

        };

        # https://github.com/prometheus-community/systemd_exporter?tab=readme-ov-file#configuration
        systemd.enable = true;
      };
    };

    graylog = {
      enable = false;
      elasticsearchHosts = [ "http://127.0.0.1:9200" ];
      package = pkgs.graylog-6_0;
      extraConfig = ''
        	http_bind_address = 0.0.0.0:9000
        	'';
    };
    mongodb = {
      enable = false;
      package = pkgs.mongodb-6_0;
    };

    # Port: 3000
    grafana = {
      # TODO: grafana-opensearch-datasource plugin
      enable = false;
      declarativePlugins = [
        pkgs.grafana-treemap-panel
      ];
      settings = {
        server = {
          enforce_domain = false;
          enable_gzip = true;
          http_addr = "0.0.0.0";
        };
        security = {
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

  };

  sops = {
    age = {
      generateKey = true;
    };
    defaultSopsFile = ../../secrets/default.yaml;
    secrets = {
      tailscale_auth_key = { };
      grafana_admin_password = {
        owner = lib.mkIf config.services.grafana.enable config.systemd.services.grafana.serviceConfig.User;
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

      jwt = {
        mode = "0444";
      };

    };
  };

  environment = {

    systemPackages = with pkgs; [
      btop
      opensearch-cli
      jellyfin
      jellyfin-web
      jellyfin-ffmpeg
      rclone
      tailscale
      vim
      sops
      age
      ffmpeg-full
      python3
    ];
  };
  security = {
    auditd.enable = true;
    audit.enable = true;
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
      ffmpeg-full = pkgs.ffmpeg-full.override {
        withUnfree = true;
        withOpengl = true;
      };

      bazarr = pkgs.bazarr.override {
        ffmpeg = pkgs.ffmpeg-full;
      };

      sonarr = pkgs.sonarr.override {
        ffmpeg = pkgs.ffmpeg-full;
        withFFmpeg = true;
      };
    };

    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
      "aspnetcore-runtime-6.0.36"
    ];

    vaapiIntel = pkgs.vaapiIntel.override { enableHybridCodec = true; };

  };

  system.stateVersion = "24.11";
}
