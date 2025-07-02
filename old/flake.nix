{
  description = "Jylhis personal flake";

  inputs = {

  };
  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
      treefmt-nix,
      ...
    }:
    let
      system = "x86_64-linux";

      pkgs-stable = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [
          emacs-overlay.overlay
          self.overlays.by-name
          self.overlays.additions
        ];
      };

      treefmtEval = treefmt-nix.lib.evalModule nixpkgs.legacyPackages.${system} ./treefmt.nix;
    in
    {
      checks.${system}.formatting = treefmtEval.config.build.check self;

      packages.${system} = import ./pkgs pkgs-stable;

      formatter.${system} = treefmtEval.config.build.wrapper;

      apps.${system} = import ./apps nixpkgs.legacyPackages.${system};

      devShells.${system}.default =
        let
          pkgs = pkgs-stable;
        in
        pkgs.mkShellNoCC {
          buildInputs = [
            # Deployment tools
            #   pkgs.nixos-anywhere
            pkgs.age
            pkgs.sops
            pkgs.ssh-to-age
            pkgs.git-agecrypt

            # Other tools
            pkgs.dconf2nix

            # SEC
            pkgs.vulnix
            pkgs.sbomnix

          ];
        };

      overlays = import ./overlays { inherit (nixpkgs) lib; };
      nixosModules = import ./modules;
      #homeManagerModules = import ./modules/home-manager;
    };
}
