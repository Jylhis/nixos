{
  config,
  ...
}:
let
  inherit (config.colorScheme) palette;
in
{
  home.file = {
    ".config/btop/themes/current.theme" = {
      text = ''
        # Main text color
        theme[main_fg]="${palette.base05}"

        # Title color for boxes
        theme[title]="${palette.base05}"

        # Highlight color for keyboard shortcuts
        theme[hi_fg]="${palette.base0D}"

        # Background color of selected item in processes box
        theme[selected_bg]="${palette.base01}"

        # Foreground color of selected item in processes box
        theme[selected_fg]="${palette.base05}"

        # Color of inactive/disabled text
        theme[inactive_fg]="${palette.base04}"

        # Misc colors for processes box including mini cpu graphs, details memory graph and details status text
        theme[proc_misc]="${palette.base0D}"

        # Cpu box outline color
        theme[cpu_box]="${palette.base0B}"

        # Memory/disks box outline color
        theme[mem_box]="${palette.base09}"

        # Net up/down box outline color
        theme[net_box]="${palette.base0E}"

        # Processes box outline color
        theme[proc_box]="${palette.base0C}"

        # Box divider line and small boxes line color
        theme[div_line]="${palette.base04}"

        # Temperature graph colors
        theme[temp_start]="${palette.base0B}"
        theme[temp_mid]="${palette.base0A}"
        theme[temp_end]="${palette.base08}"

        # CPU graph colors
        theme[cpu_start]="${palette.base0B}"
        theme[cpu_mid]="${palette.base0A}"
        theme[cpu_end]="${palette.base08}"

        # Mem/Disk free meter
        theme[free_start]="${palette.base0B}"

        # Mem/Disk cached meter
        theme[cached_start]="${palette.base0A}"

        # Mem/Disk available meter
        theme[available_start]="${palette.base09}"

        # Mem/Disk used meter
        theme[used_start]="${palette.base08}"

        # Download graph colors
        theme[download_start]="${palette.base0E}"
        theme[download_mid]="${palette.base0D}"
        theme[download_end]="${palette.base0C}"

        # Upload graph colors
        theme[upload_start]="${palette.base0E}"
        theme[upload_mid]="${palette.base0D}"
        theme[upload_end]="${palette.base0C}"
      '';
    };
  };
}
