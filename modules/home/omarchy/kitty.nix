{ config, ... }:
let
  inherit (config.colorScheme) palette;
in
{
  programs.kitty = {

    settings = {

      # Color scheme
      background = "#${palette.base00}";
      foreground = "#${palette.base05}";

      selection_foreground = "#${palette.base02}";
      selection_background = "#${palette.base00}";

      color0 = "#${palette.base00}";
      color1 = "#${palette.base08}";
      color2 = "#${palette.base0B}";
      color3 = "#${palette.base0A}";
      color4 = "#${palette.base0D}";
      color5 = "#${palette.base0E}";
      color6 = "#${palette.base0C}";
      color7 = "#${palette.base05}";
      color8 = "#${palette.base03}";
      color9 = "#${palette.base08}";
      color10 = "#${palette.base0B}";
      color11 = "#${palette.base0A}";
      color12 = "#${palette.base0D}";
      color13 = "#${palette.base0E}";
      color14 = "#${palette.base0C}";
      color15 = "#${palette.base07}";

    };

  };
}
