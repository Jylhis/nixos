{ config, inputs, ... }:
let
  inherit (config.colorScheme) palette;
  convert = inputs.nix-colors.lib.conversions.hexToRGBString;
  surfaceRgb = "rgb(${convert ", " palette.base02})";
  foregroundRgb = "rgb(${convert ", " palette.base05})";
  foregroundMutedRgb = "rgb(${convert ", " palette.base04})";
in
{
  programs.hyprlock.settings.input-field = {
    inner_color = surfaceRgb;
    outer_color = foregroundRgb; # #d3c6aa
    font_color = foregroundRgb;
    placeholder_color = foregroundMutedRgb;

  };
}
