# List of users for darwin or nixos system and their top-level configuration.
{
  lib,
  pkgs,
  config,
  self,
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
      defaultText = "All users under ./configuration/users are included by default";
      default =
        let
          dirContents = builtins.readDir (self + /configurations/home);
          fileNames = builtins.attrNames dirContents; # Extracts keys: [ "jylhis.nix" ]
          regularFiles = builtins.filter (name: dirContents.${name} == "regular") fileNames; # Filters for regular files
          baseNames = map (name: builtins.replaceStrings [ ".nix" ] [ "" ] name) regularFiles; # Removes .nix extension
        in
        baseNames;
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

    # home-manager = {
    #   useGlobalPkgs = true;
    #   useUserPackages = true;
    #   # Enable home-manager for our user
    #   # users = mapListToAttrs config.myusers (name: {
    #   #   imports =
    #   #     let
    #   #       pathToDefault = self + /configurations/home/${name}.nix;
    #   #     in
    #   #     lib.optionals (builtins.pathExists pathToDefault) [ pathToDefault ];
    #   # });
    # };

    # All users can add Nix caches.
    nix.settings.trusted-users = [
      "root"
    ]
    ++ config.myusers;
  };
}
