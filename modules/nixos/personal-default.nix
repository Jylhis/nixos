{
  lib,
  config,
  pkgs,
  ...
}:
{
  imports = [ ];
  options = {

  };
  config = {

    nix = {
      gc = {
        automatic = true;
        options = "--delete-older-than 14d";
        randomizedDelaySec = "45m";
      };
      optimise = {
        automatic = true;

      };
      settings = {
        download-attempts = 2;
        connect-timeout = 5;
        fallback = true;
        tarball-ttl = 604800;
        keep-outputs = true;
        experimental-features = [
          "nix-command"
          "flakes"
        ];
        trusted-users = [
          config.users.users.root.name
          "@${config.users.groups.wheel.name}"
        ];
      };
    };

    boot.plymouth.theme = lib.mkIf config.boot.plymouth.enable (lib.mkDefault "breeze");

    # Automatically connect any thunderbolt device
    services.udev.extraRules = lib.mkDefault ''ACTION=="add", SUBSYSTEM=="thunderbolt", ATTR{authorized}=="0", ATTR{authorized}="1"'';
    time.timeZone = lib.mkDefault "Europe/Zurich";

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

    boot.loader.systemd-boot.configurationLimit = lib.mkIf config.boot.loader.systemd-boot.enable (
      lib.mkDefault 3
    );

    # Allow unfree packages
    nixpkgs.config = {

      allowUnfree = true;

      packageOverrides = pkgs: {
        ffmpeg-full = pkgs.ffmpeg-full.override {
          withUnfree = config.nixpkgs.config.allowUnfree;
          withOpengl = true;
        };

      };
    };

    # known Long-lived public host keys
    programs.ssh.knownHostsFiles = [
      (pkgs.writeText "github.pub" ''
        github.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBEmKSENjQEezOmxkZMy7opKgwFB9nkt5YRrYMjNuG5N87uRgg6CLrbo5wAdT/y6v0mKV0U2w0WZ2YB/++Tpockg=
        github.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOMqqnkVzrm0SdG6UOoqKLsabgH5C9okWi0dh2l9GKJl
        github.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQCj7ndNxQowgcQnjshcLrqPEiiphnt+VTTvDP6mHBL9j1aNUkY4Ue1gvwnGLVlOhGeYrnZaMgRK6+PKCUXaDbC7qtbW8gIkhL7aGCsOr/C56SJMy/BCZfxd1nWzAOxSDPgVsmerOBYfNqltV9/hWCqBywINIR+5dIg6JTJ72pcEpEjcYgXkE2YEFXV1JHnsKgbLWNlhScqb2UmyRkQyytRLtL+38TGxkxCflmO+5Z8CSSNY7GidjMIZ7Q4zMjA2n1nGrlTDkzwDCsw+wqFPGQA179cnfGWOWRVruj16z6XyvxvjJwbz0wQZ75XK5tKSb7FNyeIEs4TT4jk+S4dhPeAUC5y+bDYirYgM4GC7uEnztnZyaVWQ7B381AK4Qdrwt51ZqExKbQpTUNn+EjqoTwvqNj4kqx5QUCI0ThS/YkOxJCXmPUWZbhjpCg56i+2aB6CmK2JGhn57K5mj0MNdBXA4/WnwH6XoPWJzK5Nyu2zB3nAZp+S5hpQs+p1vN1/wsjk=
      '')
      (pkgs.writeText "gitlab.pub" ''
        gitlab.com ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBFSMqzJeV9rUzU4kWitGjeR4PWSa29SPqJ1fVkhtj3Hw9xjLVXVYrU9QlYWrOLXBpQ6KWjbjTDTdDkoohFzgbEY=
        gitlab.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAfuCHKVTjquxvt6CM6tdG4SLp1Btn/nOeHHE5UOzRdf
        gitlab.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCsj2bNKTBSpIYDEGk9KxsGh3mySTRgMtXL583qmBpzeQ+jqCMRgBqB98u3z++J1sKlXHWfM9dyhSevkMwSbhoR8XIq/U0tCNyokEi/ueaBMCvbcTHhO7FcwzY92WK4Yt0aGROY5qX2UKSeOvuP4D6TPqKF1onrSzH9bx9XUf2lEdWT/ia1NEKjunUqu1xOB/StKDHMoX4/OKyIzuS0q/T1zOATthvasJFoPrAjkohTyaDUz2LN5JoH839hViyEG82yB+MjcFV5MU3N1l1QL3cVUCh93xSaua1N85qivl+siMkPGbO5xR/En4iEY6K2XPASUEMaieWVNTRCtJ4S8H+9
      '')
    ];

    documentation = lib.mkIf config.documentation.enable {
      doc.enable = true;
      nixos.enable = true;
      man.enable = true;
      dev.enable = true;
      info.enable = true;
    };

    #services.xserver.dekstopManager.gnome.enable

    environment.gnome.excludePackages =
      lib.optionals config.services.xserver.desktopManager.gnome.enable
        [
          pkgs.epiphany
          pkgs.geary
          pkgs.gnome-maps
          pkgs.gnome-music
          pkgs.gnome-weather
          pkgs.rhythmbox
          pkgs.totem
          pkgs.gnome-tour
        ];

    # Enable HEIC support
    environment.systemPackages = [
      pkgs.libheif
      pkgs.libheif.out
    ];
    environment.pathsToLink = [ "share/thumbnailers" ];

    programs.firefox.languagePacks = lib.optionals config.programs.firefox.enable [
      "en-US"
      "en-GB"
      "fi"
      "de"
      "fr"
    ];

  };
}
