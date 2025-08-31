_: {
  fonts.fontconfig = {
    enable = true;

    # User-specific font preferences prioritizing programming fonts
    defaultFonts = {
      monospace = [
        "JetBrainsMono Nerd Font"
        "FiraCode Nerd Font"
        "Iosevka Nerd Font"
        "CascadiaCode Nerd Font"
      ];
      sansSerif = [
        "Inter"
        "Noto Sans CJK SC"
      ];
      serif = [
        "Source Serif Pro"
        "Noto Serif CJK SC"
      ];
    };
  };

  # Session variables for consistent font sizing across applications
  home.sessionVariables = {
    # Standard font sizes for applications to reference
    FONT_SIZE_SMALL = "9";
    FONT_SIZE_NORMAL = "11";
    FONT_SIZE_LARGE = "13";
  };
}
