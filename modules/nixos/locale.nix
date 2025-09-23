{ lib, config, ... }:
{

  i18n = {
    defaultLocale = "en_GB.UTF-8";
    supportedLocales = [
      "all"
    ];
  };

  programs.firefox.languagePacks = lib.optionals config.programs.firefox.enable [
    "en-US"
    "en-GB"
    "fi"
    "de"
    "fr"
  ];
}
