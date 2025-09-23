{ config, pkgs, ... }:
let
  inherit (config.colorScheme) palette;
in
{
  programs.kitty = {
    enable = true;
    enableGitIntegration = config.programs.git.enable;
    shellIntegration.mode = "rc profile";
    font = {
      # name = "JetBrainsMono Nerd Font";
      name = "CaskaydiaMono Nerd Font";
      package = pkgs.nerd-fonts.caskaydia-mono;
      size = 12;
    };
    settings = {
      bold_italic_font = "auto";
      background_opacity = "0.95";
      window_padding_width = 14;
      window_padding_height = 14;
      hide_window_decorations = true;
      show_window_resize_notification = false;
      cursor_shape = "block";
      cursor_blink_interval = 0;
      confirm_os_window_close = 0;
      single_instance = true;
      allow_remote_control = true;

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

      # Tab configuration
      tab_bar_style = "powerline";
      tab_bar_edge = "bottom";
      tab_title_template = "{title}{' :{}:'.format(num_windows) if num_windows > 1 else ''}";

      # Window configuration
      inactive_text_alpha = "0.7";
      active_border_color = "none";
      remember_window_size = false;

      wayland_enable_ime = false;

      # Layout
      enabled_layouts = "splits,horizontal,vertical,tall";
      enable_audio_bell = "no";
    };
    keybindings = {
      "alt+1" = "goto_tab 1";
      "alt+2" = "goto_tab 2";
      "alt+3" = "goto_tab 3";
      "alt+4" = "goto_tab 4";
      "alt+5" = "goto_tab 5";
      "alt+6" = "goto_tab 6";
      "alt+7" = "goto_tab 7";
      "alt+8" = "goto_tab 8";
      "alt+9" = "goto_tab 9";
    };
  };
}
