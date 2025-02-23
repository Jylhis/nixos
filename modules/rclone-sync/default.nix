{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
let

  cfg = config.services.rclone-sync;
  opt = options.services.rclone-sync;

  defaultUser = "rclone-sync";
  defaultGroup = defaultUser;
in
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];

  options.services.rclone-sync = {

    enable = mkEnableOption "Enable RClone sync";

    package = mkPackageOption pkgs "rclone" { };

    source = mkOption {
      type = types.str;
      default = "";
    };

    destination = mkOption {
      type = types.str;
      default = "";
    };

    extraArgs = mkOption {
      type = with lib.types; listOf str;
      default = [ ];
      description = ''
        	  Extra arguments passed to rclone
        	'';

    };

    environment = {
      extra = mkOption {
        type = types.attrs;
        description = "Extra environment variables to pass run RClone https://rclone.org/docs/#precedence";
        default = { };
        example = {
          RCLONE_SKIP_LINKS = true;
          RCLONE_LOCAL_SKIP_LINKS = true;
          RCLONE_CONFIG_MYREMOTE_SKIP_LINKS = true;
        };
      };
      file = mkOption {
        type = types.nullOr types.path;
        description = "Systemd environment file to add to RClone.";
        default = null;
      };
    };

  };

  config = mkIf cfg.enable {
    systemd.packages = [ pkgs.rclone ];
    users.users = {
      ${cfg.user} = {
        group = cfg.group;
        home = cfg.dataDir;
        createHome = true;
        description = "RClone sync daemon user";
      };
    };

    users.groups = {

      "${cfg.group}" = { };
    };

    systemd.services = {

      description = "RClone-sync";
      after = [ "network.target" ];
      wants = [ "network.target" ];

      environment = cfg.environment.extra // {
        RCLONE_CACHE_DIR = "/var/cache/rclone-sync";
        # --ignore-existing
        # --immutable
        # --inplace
        # --links
        # --order-by string
        # --update
        # --delete-excluded
        # --exclude
        # --include
        # --track-renames
      };

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        Group = cfg.group;
        ProtectSystem = "strict";
        Restart = "on-failure";
        RestartSec = 30;
        CacheDirectory = "rclone-sync";
        CacheDirectoryMode = "0700";
        PrivateDevices = true;
        ProtectControlGroups = true;
        ProtectHome = "read-only";
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        StateDirectory = "rclone-sync";
        PrivateTmp = true;
        EnvironmentFile = lib.mkIf (cfg.environment.file != null) cfg.environment.file;

        # ExecStart = let
        #   extraArgsEscaped = lib.escapeShellArgs cfg.extraArgs;
        # in
        #   ''
        #               ${cfg.package}/bin/rclone \
        #   	    sync source:path dest:path ${extraArgsEscaped}
        # '';

      };
    };

  };
}
