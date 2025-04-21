_: {
  config = {
    nix.settings = {
      substituters = [
        "https://nix-community.cachix.org"
        "https://jylhis-nixos.cachix.org"
      ];
      trusted-substituters = [
        "https://nix-community.cachix.org"
        "https://jylhis-nixos.cachix.org"
      ];
      trusted-public-keys = [
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "jylhis-nixos.cachix.org-1:Sk7hqPdA7V0TJvwQakraPOtdPHd4vMrkunpUxub831E="
      ];
    };
  };
}
