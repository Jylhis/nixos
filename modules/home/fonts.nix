_: {
  fonts.fontconfig = {
    enable = true;

    # User-specific font preferences prioritizing programming fonts
    defaultFonts = {
      monospace = [
        "Caskaydia Mono Nerd Font"
      ];
      sansSerif = [
        "Noto Sans"
      ];
      serif = [
        "Noto Serif"
      ];
    };
  };

  # # Session variables for consistent font sizing across applications
  # home.sessionVariables = {
  #   # Standard font sizes for applications to reference
  #   FONT_SIZE_SMALL = "9";
  #   FONT_SIZE_NORMAL = "11";
  #   FONT_SIZE_LARGE = "13";
  # };
}
