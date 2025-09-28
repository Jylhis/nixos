{ config, ... }:
{
  services.mako = {
    settings = {
      background-color = "#${config.colorScheme.palette.base00}";
      text-color = "#${config.colorScheme.palette.base05}";
      border-color = "#${config.colorScheme.palette.base04}";
      progress-color = "#${config.colorScheme.palette.base0D}";
    };
  };
}
