{
  lib,
  pkgs,
  config,
  ...
}:
with lib;
let

  cfg = config.services.cloud-sync;
  opt = options.services.cloud-sync;

  defaultUser = "cloud-sync";
  defaultGroup = defaultUser;
in
{
  imports = [
    # Paths to other modules.
    # Compose this module out of smaller ones.
  ];

  options = {
    # Option declarations.
    # Declare what settings a user of this module can set.
    # Usually this includes a global "enable" option which defaults to false.
    services.cloud-sync = {
      enable = mkEnableOption "Enable cloud sync";
      package = mkPackageOption pkgs "rclone" { };
      onedrive = mkOption {
        default = { };
        description = "Settings for OneDrive";

      };
    };
  };
  # https://github.com/NixOS/nixpkgs/blob/a2e485e7c40448382a4f87ecb2c5aab5593cc782/nixos/modules/services/networking/syncthing.nix
  config = mkIf cfg.enable {
    # Option definitions.
    # Define what other settings, services and resources should be active.
    # Usually these depend on whether a user of this module chose to "enable" it
    # using the "option" above.
    # Options for modules imported in "imports" can be set here.
    systemd.packages = [ pkgs.rclone ];
    users.users = {
      ${defaultUser} = {
        group = cfg.group;
        home = cfg.dataDir;
        createHome = true;
        #uid = config.ids.uids.syncthing;
        description = "cloud-sync daemon user";
      };
    };

    users.groups = {
      #${defaultGroup}.gid = config.ids.gids.cloud-sync;
    };

    systemd.services = {
      description = "Cloud-sync";
      after = [ "network-online.target" ];
      wants = [ "network-online.target" ];
      serviceConfig = {
        User = cfg.user;
        Group = cfg.group;
        ExecStartPre = "-/usr/bin/mkdir -p %h/mnt/%i";
        ExecStart = ''
            ${pkgs.rclone}/bin/rclone sync --differ source:path dest:path [flags]
          	'';
        # --ignore-existing
        # --immutable
        # --inplace
        # --links
        # --order-by string
        # --update
        # --delete-excluded
        # --exclude
        # --include

        # [personal]
        # type = onedrive
        # client_id = clientid
        # client_secret = clientsecret
        # token = oauth
        # auth_url = authurl
        # token_url = tokenurl
        # drive_id = driveid
        # drive_type = personal
        # access_scopes = Files.Read Files.Read.All Sites.Read.All offline_access
        # av_override = true
        # delta = true # ONLY IF FROM THE ROOT
        # metadata_permissions = read

      };
    };

  };
}
