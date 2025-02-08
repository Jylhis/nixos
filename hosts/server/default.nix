# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).
{
  pkgs,
  config,
  ...
}:
{
  imports = [
    ./disko-config.nix
    # ./hardware-configuration.nix
  ];

  disko.devices.disk.main.device = "/dev/sda";
  disko.devices.disk.secondary.device = "/dev/sdb";

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
        efiSupport = true;
        efiInstallAsRemovable = true;
      };
    };

    initrd.systemd.enable = true;

  };

  networking = {
    hostId = "82a30203";
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

  #console.useXkbConfig = true;

  services = {
    openssh.enable = true;
    tailscale = {
      enable = false;
      useRoutingFeatures = "server";
      disableTaildrop = true;
    };

    sonarr.enable = false; # port: 8989
    radarr.enable = false; # port: 7878
    lidarr.enable = false; # port: 8686
    bazarr.enable = false; # port: 6767
    prowlarr.enable = false; # port: 9696
    readarr.enable = false; # port: 8787
    jellyfin.enable = false; # port: https: 8920 & http: 8096

    syncthing = {
      enable = false;
      openDefaultPorts = true;
      settings.gui = {
        user = "myuser";
        password = "mypassword";
      };
    };

    # Monitoring

    # port: 9090
    prometheus = {
      enable = false;
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

                "localhost:${toString config.services.prometheus.exporters.exportarr-radarr.port}"
                "localhost:${toString config.services.prometheus.exporters.exportarr-sonarr.port}"
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
        node.enable = true;
        #   ping.enable = true;
        process.enable = true;
        systemd.enable = true;
        #   zfs.enable = true;
        #   wireguard.enable = true;
      };
    };

    # Port: 3000
    grafana = {
      enable = false;
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
          serve_from_sub_path = true;
          root_url = "%(protocol)s://%(domain)s:%(http_port)s/grafana/";
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
        enable = false;
        python.enable = true;
        python.recommendedPythonPackages = true;
      };
    # Experimental
    #shiori.enable = true;
    #outline.enable = true;
    #your_spotify.enable = true;

    # Reverse proxy
    nginx.enable = false;
    nginx.virtualHosts.localhost = {
      addSSL = false;
      enableACME = false;
      locations."/grafana/" = {
        proxyPass = "http://${toString config.services.grafana.settings.server.http_addr}:${toString config.services.grafana.settings.server.http_port}";
        proxyWebsockets = true;
        recommendedProxySettings = true;
      };
    };
  };

  # sops = {
  #   age = {
  #     keyFile = "${config.users.users.root.home}/.config/sops/age/keys.txt";
  #     generateKey = true;
  #     # sshKeyPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
  #   };
  #   defaultSopsFile = ../../secrets/server.yaml;
  #   #secrets = {
  #   #  zfs_key = { };
  #   #};
  # };

  environment = {
    systemPackages = with pkgs; [
      sops
      age
    ];
  };

  # Allow unfree packages
  nixpkgs.config = {
    allowUnfree = true;
    permittedInsecurePackages = [
      "dotnet-sdk-6.0.428"
      "aspnetcore-runtime-6.0.36"
    ];
  };

  system.stateVersion = "24.11";
}
