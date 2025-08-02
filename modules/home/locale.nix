{ pkgs, ... }:
{
  home = {
    packages = with pkgs; [
      (aspellWithDicts (
        dicts: with dicts; [
          en
          en-computers
          en-science
          sv
          fi
          de
          fr
        ]
      ))
    ];
  };

  # Use English language with Finnish time formats and Swiss currency
  home.language = {
    base = "en_GB.UTF-8";
    time = "fi_FI.UTF-8";
    monetary = "de_CH.UTF-8";
  };
}
