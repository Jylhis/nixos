# List of users for darwin or nixos system and their top-level configuration.
{
  flake,
  pkgs,
  lib,
  config,
  ...
}:
let
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
    myusers = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      description = "List of usernames";
    };
  };

  config = {
    # For home-manager to work.
    # https://github.com/nix-community/home-manager/issues/4026#issuecomment-1565487545
    users.users = mapListToAttrs config.myusers (
      name:
      lib.optionalAttrs pkgs.stdenv.isDarwin {
        home = "/Users/${name}";
      }
      // lib.optionalAttrs pkgs.stdenv.isLinux {
        isNormalUser = true;
        openssh.authorizedKeys.keys = config.home-manager.users.${name}.me.publicKeys;

        # openssh.authorizedKeys.keyFiles =
        # let
        #   pathToKey = self + /secrets/${name}.pub;
        # in
        # lib.optionals (builtins.pathExists pathToKey) [ pathToKey ];
      }
    );

    home-manager = {
      useGlobalPkgs = true;
      useUserPackages = true;
    };
    # Enable home-manager for our user
    home-manager.users = mapListToAttrs config.myusers (_name: {
      # imports = [ (self + /configurations/home/${name}.nix) ];
    });

    # All users can add Nix caches.
    nix.settings.trusted-users = [
      "root"
    ]
    ++ config.myusers;
  };
}
