# List of users for darwin or nixos system and their top-level configuration.
{
  flake,
  pkgs,
  lib,
  config,
  ...
}:
let
  inherit (flake.inputs) self;
  mapListToAttrs =
    m: f:
    lib.listToAttrs (
      map (name: {
        inherit name;
        value = f name;
      }) m
    );
in
{
  options = {
    home-manager.enable = lib.mkEnableOption "home-manager manager users";
    myusers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "List of usernames";
      defaultText = "All users under ./configuration/users are included by default";
      default =
        let
          dirContents = builtins.readDir (self + /configurations/home);
          fileNames = builtins.attrNames dirContents;

          # Get users from .nix files
          regularFiles = builtins.filter (name: dirContents.${name} == "regular") fileNames;
          nixFiles = builtins.filter (name: lib.hasSuffix ".nix" name) regularFiles;
          usersFromFiles = map (name: builtins.replaceStrings [ ".nix" ] [ "" ] name) nixFiles;

          # Get users from directories
          directories = builtins.filter (name: dirContents.${name} == "directory") fileNames;

          # Combine both lists
          allUsers = usersFromFiles ++ directories;
        in
        allUsers;
    };
  };

  config = {
    users.users = mapListToAttrs config.myusers (
      name:
      lib.optionalAttrs pkgs.stdenv.isDarwin {
        home = "/Users/${name}";
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        isNormalUser = true;
        openssh.authorizedKeys.keys =
          let
            pathToKey = self + /secrets/${name}.pub;
          in
          config.home-manager.users.${name}.me.publicKeys
            or (lib.optionals (builtins.pathExists pathToKey) [ pathToKey ]);

      }
    );

    home-manager = lib.mkIf config.home-manager.enable {
      useGlobalPkgs = true;
      useUserPackages = true;
      backupFileExtension = "backup";
      extraSpecialArgs = {
        inherit (flake) inputs;
      };
      # Enable home-manager for our user
      users = mapListToAttrs config.myusers (name: {
        imports = [
          (self + /modules/home)
        ]
        ++ lib.optional (builtins.pathExists (self + /configurations/home/${name}.nix)) (
          self + /configurations/home/${name}.nix
        )
        ++ lib.optional (builtins.pathExists (self + /configurations/home/${name})) (
          self + /configurations/home/${name}
        );
      });
    };

    # All users can add Nix caches.
    nix.settings.trusted-users = [
      "root"
    ]
    ++ config.myusers;
  };
}
