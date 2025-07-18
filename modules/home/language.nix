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
    language = {
      base = "en_US.UTF-8";
      collate = "de_CH.UTF-8";
      ctype = "de_CH.UTF-8";
      measurement = "de_CH.UTF-8";
      messages = "en_US.UTF-8";
      monetary = "de_CH.UTF-8";
      name = "de_CH.UTF-8";
      numeric = "de_CH.UTF-8";
      paper = "de_CH.UTF-8";
      telephone = "de_CH.UTF-8";
      time = "de_CH.UTF-8";
    };
  };
}
