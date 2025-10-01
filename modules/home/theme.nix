{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  nix-colors-lib = inputs.nix-colors.lib.contrib { inherit pkgs; };
  wallpaper-dark = "1-Pawel-Czerwinski-Abstract-Purple-Blue.jpg";
  wallpaper-light = "kanagawa-1.png";
  wallpaper = if enable-dark then wallpaper-dark else wallpaper-light;
  generated = false;
  color-dark = inputs.nix-colors.lib.schemeFromYAML "modus-operandi-tinted" (
    builtins.readFile ../../theme/base16/modus_operandi_tinted.yaml
  );
  color-light = inputs.nix-colors.lib.schemeFromYAML "modus-operandi-tinted" (
    builtins.readFile ../../theme/base16/modus_vivendi_tinted.yaml
  );
  color-generated = nix-colors-lib.colorSchemeFromPicture {
    path = ../../theme/wallpaper/${wallpaper};
    variant = if enable-dark then "dark" else "light";
  };
  enable-dark = false;
in
{
  imports = [ inputs.nix-colors.homeManagerModules.default ];
  config = {
    specialisation.dark.configuration = {
      dconf.settings."org/gnome/desktop/interface".color-scheme = lib.mkForce "prefer-dark";
      colorScheme = lib.mkForce color-dark;
    };

    # Gnome stuff
    dconf.settings."org/gnome/desktop/background" = {
      picture-uri = "file://${config.home.homeDirectory}/Pictures/Wallpapers/${wallpaper-light}";
      picture-uri-dark = "file://${config.home.homeDirectory}/Pictures/Wallpapers/${wallpaper-dark}";
    };
    dconf.settings."org/gnome/desktop/interface".color-scheme =
      if enable-dark then "prefer-dark" else "prefer-light";

    colorScheme =
      if generated then
        color-generated
      else if enable-dark then
        color-dark
      else
        color-light;
  };
}
