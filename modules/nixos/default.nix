_: {

  config = {
    services = {
      openssh.enable = true;
      tailscale.enable = true;
    };

    nix = {
      settings = {
        substituters = [
          "https://jylhis-nixos.cachix.org"
        ];
        trusted-substituters = [
          "https://jylhis-nixos.cachix.org"
        ];
        trusted-public-keys = [
          "jylhis-nixos.cachix.org-1:Sk7hqPdA7V0TJvwQakraPOtdPHd4vMrkunpUxub831E="
        ];
      };
    };

  };
}
