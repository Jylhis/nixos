{ pkgs, ... }:
{
  fonts = {
    enableDefaultPackages = true;
    fontDir.enable = true;

    # Curated font selection for optimal performance and coverage
    packages = with pkgs; [
      # Programming fonts (Nerd Font variants only)
      nerd-fonts.jetbrains-mono
      nerd-fonts.fira-code
      nerd-fonts.iosevka
      nerd-fonts.caskaydia-mono

      # UI and reading fonts
      inter # Modern UI font
      source-serif-pro # High-quality serif font

      # CJK and international support
      noto-fonts-cjk-sans # Chinese, Japanese, Korean sans-serif
      noto-fonts-cjk-serif # Chinese, Japanese, Korean serif
      noto-fonts-emoji # Emoji support

      # Icon and symbol fonts
      font-awesome # Icon font
      stix-two # Mathematical typesetting
    ];

    fontconfig = {
      enable = true;
      cache32Bit = true; # Better performance

      # Default font families for consistent rendering
      defaultFonts = {
        serif = [
          "Source Serif Pro"
          "Noto Serif CJK SC"
        ];
        sansSerif = [
          "Inter"
          "Noto Sans CJK SC"
        ];
        monospace = [
          "JetBrainsMono Nerd Font"
          "FiraCode Nerd Font"
          "Iosevka Nerd Font"
        ];
        emoji = [ "Noto Color Emoji" ];
      };

      # Optimized rendering settings
      antialias = true;
      hinting = {
        enable = true;
        style = "slight";
        autohint = false;
      };

      subpixel = {
        rgba = "rgb";
        lcdfilter = "default";
      };
    };
  };
}
