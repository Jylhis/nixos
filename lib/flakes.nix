{
  inputs,
  self,
  lib,
  ...
}:
rec {
  # Special arguments builder with safety checks
  specialArgsFor = rec {
    common = {
      flake = {
        inherit self;
        inputs = inputs // {
          inherit self;
        };
      };
      inherit inputs self;
      inherit (inputs) nix-darwin home-manager;
    };

    nixos = _system: common;

    darwin =
      _system:
      common
      // {
        rosettaPkgs = inputs.nixpkgs.legacyPackages."x86_64-darwin" or null;
      };
  };

  # System builders with existence checks
  mkLinuxSystem =
    {
      home-manager ? false,
    }:
    module:
    inputs.nixpkgs.lib.nixosSystem {
      system = "x86_64-linux"; # Default, can be overridden
      modules = [
        # Add nixpkgs configuration with overlay
        {
          nixpkgs.overlays = [ self.overlays.default ];
          nixpkgs.config.allowUnfree = true;
        }
        module
      ]
      ++ lib.optional (builtins.pathExists "${self}/modules/nixos") "${self}/modules/nixos"
      ++ lib.optionals home-manager [ inputs.home-manager.nixosModules.home-manager ];
      specialArgs = specialArgsFor.nixos "x86_64-linux";
    };

  mkMacosSystem =
    {
      home-manager ? false,
    }:
    module:
    inputs.nix-darwin.lib.darwinSystem {
      system = "aarch64-darwin"; # Default for M1
      modules = [
        # Add nixpkgs configuration with overlay
        {
          nixpkgs.overlays = [ self.overlays.default ];
          nixpkgs.config.allowUnfree = true;
        }
        module
      ]
      ++ lib.optional (builtins.pathExists "${self}/modules/darwin") "${self}/modules/darwin"
      ++ lib.optionals home-manager [ inputs.home-manager.darwinModules.home-manager ];
      specialArgs = specialArgsFor.darwin "aarch64-darwin";
    };

  mkHomeConfiguration =
    pkgs: module:
    inputs.home-manager.lib.homeManagerConfiguration {
      inherit pkgs;
      modules = [
        module
      ]
      ++ lib.optional (builtins.pathExists "${self}/modules/home") "${self}/modules/home";
      extraSpecialArgs = {
        inherit inputs; # Pass ALL inputs, not partial
        # emacsJ now available via pkgs.emacsJ through overlay
      };
    };
  # Safe file discovery with graceful handling
  mapAttrsMaybe =
    f: attrs:
    lib.pipe attrs [
      (lib.mapAttrsToList f)
      (builtins.filter (x: x != null))
      builtins.listToAttrs
    ];

  forAllNixFiles =
    dir: f:
    if builtins.pathExists dir then
      lib.pipe dir [
        builtins.readDir
        (mapAttrsMaybe (
          fn: type:
          if type == "regular" && lib.hasSuffix ".nix" fn then
            lib.nameValuePair (lib.removeSuffix ".nix" fn) (f "${dir}/${fn}")
          else if type == "directory" && builtins.pathExists "${dir}/${fn}/default.nix" then
            lib.nameValuePair fn (f "${dir}/${fn}")
          else
            null
        ))
      ]
    else
      { };

}
