{
  perSystem =
    { pkgs, ... }:
    {
      devShells.default = pkgs.mkShellNoCC {
        name = "nixos-unified-template-shell";
        meta.description = "Shell environment for modifying this Nix configuration";
        packages = with pkgs; [
          just
          nixd

          # Other tools
          dconf2nix

          # SEC
          vulnix
          sbomnix
        ];
      };
    };
}
