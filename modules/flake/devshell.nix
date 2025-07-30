{
  perSystem =
    { pkgs, ... }:
    {
      devShells.default = pkgs.mkShellNoCC {
        name = "jylhis-devshell";
        meta.description = "Shell environment for modifying this Nix configuration";
        packages = with pkgs; [
          just
          nixd
          # Deployment tools
          deploy-rs
          #   pkgs.nixos-anywhere
          age
          sops
          ssh-to-age
          git-agecrypt

          # SEC
          vulnix
          sbomnix
        ];
      };
    };
}
