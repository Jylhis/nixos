# TODO:
# - Lock screen block background
# - Monitor management
# - Keybindings and workspace management (per monitor?)
# - Darkmode/lightmode
# - Autostart: 1password
# - windows to fix:
#   - Firefox calendar detail popup
{
  lib,
  config,
  pkgs,
  ...
}:
{
  config = lib.mkIf config.wayland.windowManager.hyprland.enable {
    # Rofi configuration
    programs.rofi = {
      enable = true;
      package = pkgs.rofi-wayland;
      extraConfig = {
        modi = "drun";
        font = "HarmonyOS Sans SC bold 12";
        show-icons = true;
        icon-theme = "Reversal-dark";
        display-drun = "";
        drun-display-format = "{name}";
        sidebar-mode = false;
      };
      theme = {
        "*" = {
          bg = "#181825";
          fg = "#e8e8d3";
          accent = "#687363";
          button = "#31344a";
          background-color = "@bg";
          text-color = "@fg";
        };

        window = {
          border-radius = "5px";
          width = "50%";
          padding = "28px";
        };

        prompt = {
          background-color = "@button";
          enabled = true;
          padding = "0.5% 32px 0% -0.5%";
          font = "CaskaydiaCove Nerd Font bold 12";
        };

        entry = {
          placeholder = "Search";
          background-color = "@button";
          placeholder-color = "@fg";
          expand = true;
          padding = "0.15% 0% 0% 0%";
        };

        inputbar = {
          children = [
            "prompt"
            "entry"
          ];
          background-color = "@button";
          expand = false;
          border-radius = "6px";
          margin = "0%";
          padding = "10px";
        };

        listview = {
          columns = 4;
          lines = 3;
          cycle = false;
          dynamic = true;
          layout = "vertical";
        };

        mainbox = {
          children = [
            "inputbar"
            "listview"
          ];
          spacing = "2%";
          padding = "2% 1% 2% 1%";
        };

        element = {
          orientation = "vertical";
          padding = "2% 0% 2% 0%";
        };

        element-icon = {
          size = "48px";
          horizontal-align = "0.5";
        };

        element-text = {
          expand = true;
          horizontal-align = "0.5";
          vertical-align = "0.5";
          margin = "0.5% 0.5% -0.5% 0.5%";
        };

        "element-text, element-icon" = {
          background-color = "inherit";
          text-color = "inherit";
        };

        "element selected" = {
          background-color = "@button";
          border-radius = "6px";
        };
      };
    };
    wayland.windowManager.hyprland = {
      settings = {
        # Monitor configuration
        monitor = [
          ",preferred,auto,1"
        ];

        # Input configuration - matching Plasma setup
        input = {
          kb_layout = "us,fi";
          kb_options = "ctrl:swapcaps,grp:win_space_toggle";

          follow_mouse = 1;
          accel_profile = "flat";
          touchpad = {
            natural_scroll = true;
            disable_while_typing = true;
          };

          sensitivity = 0;
        };

        gestures = {
          workspace_swipe = false;
        };

        misc = {
          force_default_wallpaper = lib.mkDefault false;
          disable_hyprland_logo = lib.mkDefault false;
        };

        # General settings
        general = {
          gaps_in = 6;
          gaps_out = 12;
          border_size = 2;
          "col.active_border" = "rgba(ff00ffcc) rgba(00ffffcc) 135deg";
          "col.inactive_border" = "rgba(00000000)";
          resize_on_border = true;
          no_border_on_floating = true;

          layout = "dwindle";
          allow_tearing = false;
        };

        # Decoration
        decoration = {
          rounding = 12;

          active_opacity = 1.0;
          inactive_opacity = 1.0;

          shadow = {
            enabled = false;
            range = 12;
            render_power = 6;
            color = "rgba(00000088)";
          };

          blur = {
            enabled = true;
            size = 8;
            passes = 2;
            new_optimizations = true;
            ignore_opacity = false;
            vibrancy = 0.25;
          };
        };

        # Animations
        animations = {
          enabled = true;

          bezier = [
            "myBezier, 0.05, 0.9, 0.1, 1.05"
            "overshot, 0.13, 0.99, 0.29, 1.1"
          ];

          animation = [
            "windows, 1, 3, myBezier"
            "windowsOut, 1, 5, myBezier, popin 80%"
            "windowsMove, 1, 5, myBezier"
            "border, 1, 10, default"
            "fade, 1, 5, default"
            "workspaces, 1, 4, overshot, slidevert"
          ];
        };

        # Layout configuration
        dwindle = {
          pseudotile = true;
          preserve_split = true;
        };

        master = {
          new_status = "master";
        };

        # Window rules
        windowrulev2 = [
          "suppressevent maximize, class:.*"
          "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"
        ];

        # Key bindings - matching Plasma shortcuts where possible
        "$mod" = "SUPER";

        bind = [
          # Basic applications
          "$mod, Return, exec, kitty"
          "$mod, B, exec, brave"
          "$mod, M, exec, emacsclient -c -a emacs"
          "$mod, T, exec, kitty"
          "$mod, E, exec, dolphin"
          "$mod, G, exec, emacsclient -cF '((visibility . nil))' -e '(emacs-run-launcher)'"

          # Application launcher - matching Plasma's Meta key
          "$mod, R, exec, rofi -show drun"

          # Window management - matching Plasma shortcuts
          "$mod, Q, killactive"
          "ALT, F4, killactive"
          "$mod CTRL, Escape, exec, hyprctl kill"
          "$mod, V, togglefloating"
          "$mod, P, pseudo"
          "$mod, J, togglesplit"

          # Window focus - matching Plasma's Meta+Alt+Arrow
          "$mod, Left, movefocus, l"
          "$mod, Right, movefocus, r"
          "$mod, Up, movefocus, u"
          "$mod, Down, movefocus, d"

          # Window tiling - matching Plasma's Meta+Arrow
          "$mod ALT, Left, movewindow, l"
          "$mod ALT, Right, movewindow, r"
          "$mod ALT, Up, movewindow, u"
          "$mod ALT, Down, movewindow, d"

          # Window maximize/minimize
          "$mod, Page_Up, fullscreen, 0" # Maximize Window
          # "$mod, Page_Down, movetoworkspacesilent, special" # Window Minimize equivalent

          # Workspace switching
          "$mod, 1, workspace, 1"
          "$mod, 2, workspace, 2"
          "$mod, 3, workspace, 3"
          "$mod, 4, workspace, 4"

          # Move windows to workspaces
          "$mod SHIFT, 1, movetoworkspace, 1"
          "$mod SHIFT, 2, movetoworkspace, 2"
          "$mod SHIFT, 3, movetoworkspace, 3"
          "$mod SHIFT, 4, movetoworkspace, 4"

          # Special workspace (scratchpad)
          "$mod, S, togglespecialworkspace, magic"
          "$mod ALT, S, movetoworkspace, special:magic"

          # Scroll through workspaces with scroll
          "$mod, mouse_down, workspace, e+1"
          "$mod, mouse_up, workspace, e-1"

          # Window switching - matching Plasma's Alt+Tab
          "ALT, Tab, cyclenext"
          "ALT SHIFT, Tab, cyclenext, prev"

          # Session management
          "$mod, L, exec, hyprlock"
          "CTRL ALT, Delete, exec, systemctl poweroff"

          # Clipboard history
          "$mod SHIFT, V, exec, cliphist list | rofi -dmenu | cliphist decode | wl-copy"

          # Screenshots
          "$mod SHIFT, S, exec, grim -g \"$(slurp)\" ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png"
          "ALT, Print, exec, grim ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png"
          ", Print, exec, grim -g \"$(slurp)\" ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png"
          "SHIFT, Print, exec, grim ~/Pictures/screenshot-$(date +%Y%m%d-%H%M%S).png"

          # Volume control
          ", XF86AudioRaiseVolume, exec, wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"
          ", XF86AudioLowerVolume, exec, wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"
          ", XF86AudioMute, exec, wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"
          ", XF86AudioMicMute, exec, wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"

          # Brightness control
          ", XF86MonBrightnessUp, exec, brightnessctl s 10%+"
          ", XF86MonBrightnessDown, exec, brightnessctl s 10%-"

          # Media control
          ", XF86AudioPlay, exec, playerctl play-pause"
          ", XF86AudioNext, exec, playerctl next"
          ", XF86AudioPrev, exec, playerctl previous"
          ", XF86AudioPause, exec, playerctl play-pause"
        ];

        # Mouse bindings
        bindm = [
          "$mod, mouse:272, movewindow"
          "$mod, mouse:273, resizewindow"
        ];

        # Workspace configuration
        workspace = [
          "1, monitor:eDP-1, default:true"
          "2, monitor:eDP-1"
          "3, monitor:eDP-1"
          "4, monitor:eDP-1"
        ];

        # Environment variables
        env = [
          "XCURSOR_SIZE,24"
          "HYPRCURSOR_SIZE,24"
          "QT_QPA_PLATFORMTHEME,qt5ct"
        ];

        # Autostart
        exec-once = [
          "hyprpanel"
          # "waybar"
          # "mako"
          "hypridle"
          "hyprpaper"
          "wl-paste --type text --watch cliphist store"
          "wl-paste --type image --watch cliphist store"
          # "gammastep -l 60.1699:24.9384"
          "/usr/lib/polkit-kde-authentication-agent-1"
          "nm-applet"
          "1password --silent"
          "kdeconnect-indicator"
        ];
      };
    };

    programs.kitty = {
      enable = true;
      font = {
        name = "CaskaydiaCove Nerd Font";
        size = 14;
      };
      settings = {
        background_opacity = "1.0";
        window_padding_width = 15;
        cursor_shape = "beam";
        cursor_blink_interval = 0;
        hide_window_decorations = "no";
        scrollback_lines = 2000;

        # Color scheme
        background = "#1e1e2e";
        foreground = "#abb2bf";

        # Normal colors
        color0 = "#3f4451";
        color1 = "#e06c75";
        color2 = "#98c379";
        color3 = "#d19a66";
        color4 = "#61afef";
        color5 = "#c678dd";
        color6 = "#56b6c2";
        color7 = "#e6e6e6";

        # Bright colors
        color8 = "#4f5666";
        color9 = "#ff7b86";
        color10 = "#b1e18b";
        color11 = "#efb074";
        color12 = "#67cdff";
        color13 = "#e48bff";
        color14 = "#63d4e0";
        color15 = "#ffffff";

        # Tab configuration
        tab_bar_style = "powerline";

        # Window configuration
        window_border_width = "0.5pt";
        draw_minimal_borders = "yes";
        inactive_text_alpha = "0.7";
        active_border_color = "none";

        # Layout
        enabled_layouts = "splits";
        enable_audio_bell = "no";
      };
      keybindings = {
        "alt+t" = "new_tab_with_cwd !neighbor";
        "alt+s" = "next_tab";
        "alt+a" = "previous_tab";
        "alt+w" = "close_tab";
        "ctrl+alt+s" = "set_tab_title";
        "alt+shift+left" = "move_tab_backward";
        "alt+shift+right" = "move_tab_forward";
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

    programs.hyprpanel = {
      enable = true;
      systemd.enable = true;
    };

    # Waybar configuration
    programs.waybar = {
      enable = false;
      settings = {
        mainBar = {
          layer = "top";
          position = "top";
          height = 45;
          spacing = 0;

          modules-left = [
            "hyprland/workspaces"
            "tray"
            "custom/lock"
            "custom/reboot"
            "custom/power"
            "clock"
          ];
          modules-center = [ "hyprland/window" ];
          modules-right = [
            "network"
            "battery"
            "bluetooth"
            "pulseaudio"
            "backlight"
            "custom/temperature"
            "memory"
            "cpu"
          ];

          "hyprland/workspaces" = {
            disable-scroll = false;
            all-outputs = true;
            format = "{icon}";
            on-click = "activate";
            persistent-workspaces = {
              "*" = [
                1
                2
                3
                4
                5
                6
                7
              ];
            };
            format-icons = {
              "1" = "󰣇";
              "2" = "";
              "3" = "";
              "4" = "󰇮";
              "5" = "";
              "6" = "";
              "7" = "";
              "active" = "󱓻";
            };
          };

          "custom/lock" = {
            format = "  ";
            on-click = "hyprlock";
            tooltip = true;
            tooltip-format = "Lock Screen";
          };

          "custom/reboot" = {
            format = "  ";
            on-click = "systemctl reboot";
            tooltip = true;
            tooltip-format = "Reboot";
          };

          "custom/power" = {
            format = "  ";
            on-click = "systemctl poweroff";
            tooltip = true;
            tooltip-format = "Shutdown";
          };

          network = {
            format-wifi = " 󰤨 {essid} ";
            format-ethernet = " Wired ";
            tooltip-format = "<span color='#FF1493'> 󰅧 </span>{bandwidthUpBytes}  <span color='#00BFFF'> 󰅢 </span>{bandwidthDownBytes}";
            format-linked = " 󱘖 {ifname} (No IP) ";
            format-disconnected = "  Disconnected ";
            format-alt = " 󰤨 {signalStrength}% ";
            interval = 1;
          };

          battery = {
            states = {
              warning = 30;
              critical = 15;
            };
            format = " {icon} {capacity}% ";
            format-charging = " 󱐋{capacity}%";
            interval = 1;
            format-icons = [
              "󰂎"
              "󰁼"
              "󰁿"
              "󰂁"
              "󰁹"
            ];
            tooltip = true;
          };

          bluetooth = {
            format = "  {status} ";
            format-connected = "  {device_alias} ";
            format-connected-battery = "  {device_alias}{device_battery_percentage}% ";
            tooltip-format = "{controller_alias}\t{controller_address}\n\n{num_connections} connected";
            tooltip-format-connected = "{controller_alias}\t{controller_address}\n\n{num_connections} connected\n\n{device_enumerate}";
            tooltip-format-enumerate-connected = "{device_alias}\t{device_address}";
            tooltip-format-enumerate-connected-battery = "{device_alias}\t{device_address}\t{device_battery_percentage}%";
          };

          pulseaudio = {
            format = "{icon}{volume}% ";
            format-muted = " 󰖁 0% ";
            format-icons = {
              headphone = "  ";
              hands-free = "  ";
              headset = "  ";
              phone = "  ";
              portable = "  ";
              car = "  ";
              default = [
                "  "
                "  "
                "  "
              ];
            };
            on-click-right = "pavucontrol -t 3";
            on-click = "pactl -- set-sink-mute 0 toggle";
            tooltip = true;
            tooltip-format = "Volume: {volume}%";
          };

          backlight = {
            device = "intel_backlight";
            format = "{icon}{percent}% ";
            tooltip = true;
            tooltip-format = "Brightness: {percent}%";
            format-icons = [
              " 󰃞 "
              " 󰃝 "
              " 󰃟 "
              " 󰃠 "
            ];
          };

          "custom/temperature" = {
            exec = "sensors | awk '/^Package id 0:/ {print int($4)}'";
            format = " {}°C ";
            interval = 5;
            tooltip = true;
            tooltip-format = "CPU Temperature: {}°C";
          };

          memory = {
            format = "  {used:0.1f}G/{total:0.1f}G ";
            tooltip = true;
            tooltip-format = "Memory Usage: {used:0.2f}G/{total:0.2f}G";
          };

          cpu = {
            format = "  {usage}% ";
            tooltip = true;
          };

          clock = {
            interval = 1;
            timezone = "Europe/Helsinki";
            format = "  {:%H:%M} ";
            tooltip = true;
            tooltip-format = "{:%Y-%m-%d, %A}";
          };

          tray = {
            icon-size = 17;
            spacing = 6;
          };
        };
      };
      style = ''
        * {
          font-family: "CaskaydiaCove Nerd Font", "Font Awesome 6 Free", "Font Awesome 6 Free Solid";
          font-size: 16px;
          border-radius: 0;
          min-height: 0;
          border: none;
          font-weight: bold;
        }

        #workspaces {
          background-color: rgba(24,24,37,1.0);
          border: none;
          box-shadow: none;
        }

        #tray {
          margin: 6px 3px;
          background-color: rgba(36, 36, 52, 1.0);
          padding: 6px 12px;
          border-radius: 6px;
          border-width: 0px;
        }

        #waybar {
          background-color: #181825;
          transition-property: background-color;
          transition-duration: 0.5s;
        }

        #window,
        #clock,
        #custom-power,
        #custom-reboot,
        #bluetooth,
        #battery,
        #pulseaudio,
        #backlight,
        #custom-temperature,
        #memory,
        #cpu,
        #network,
        #custom-lock {
          border-radius: 4px;
          margin: 6px 3px;
          padding: 6px 12px;
          background-color: #1e1e2e;
          color: #181825;
        }

        #clock { background-color: #89b4fa; }
        #custom-power { background-color: #f38ba8; }
        #custom-reboot { background-color: #a6e3a1; }
        #bluetooth { background-color: #f9e2af; }
        #battery { background-color: #cba6f7; }
        #pulseaudio { background-color: #89dceb; }
        #backlight { background-color: #a6a3a1; }
        #custom-temperature { background-color: #74c7ec; }
        #memory { background-color: #f7768e; }
        #cpu { background-color: #f38ba8; }
        #network { background-color: #fab387; }
        #custom-lock { background-color: #94e2d5; }
        #window { background-color: #74c7ec; }

        #waybar.hidden {
          opacity: 0.5;
        }

        #workspaces button {
          all: initial;
          min-width: 0;
          box-shadow: inset 0 -3px transparent;
          padding: 6px 18px;
          margin: 6px 3px;
          border-radius: 4px;
          background-color: rgba(36, 36, 52, 1.0);
          color: #cdd6f4;
        }

        #workspaces button.active {
          color: #1e1e2e;
          background-color: #cdd6f4;
        }

        #workspaces button:hover {
          box-shadow: inherit;
          text-shadow: inherit;
          color: #1e1e2e;
          background-color: #cdd6f4;
        }

        tooltip {
          border-radius: 8px;
          padding: 16px;
          background-color: #131822;
          color: #C0C0C0;
        }

        tooltip label {
          padding: 5px;
          background-color: #131822;
          color: #C0C0C0;
        }
      '';
    };

    services.hyprsunset.enable = true;
    # Mako notification daemon (disabled - HyprPanel handles notifications)
    services.mako = {
      enable = false;
      settings = {
        sort = "-time";
        layer = "overlay";
        width = 450;
        height = 150;
        icons = 1;
        max-icon-size = 64;
        font = "CaskaydiaCove Nerd Font 14";
        margin = "16";
        padding = "12,20";
        background-color = "#1e1e2e";
        text-color = "#ffffff";
        border-size = 2;
        border-color = "#6C3483";
        border-radius = 12;
        default-timeout = 5000;
        ignore-timeout = 0;
      };

    };

    # Hyprlock screen locker
    programs.hyprlock = {
      enable = true;
      settings = {
        general = {
          disable_loading_bar = false;
          grace = 0;
          hide_cursor = true;
          ignore_empty_input = true;
          no_fade_in = false;
        };

        background = [
          {
            path = ""; # TODO
            blur_passes = 0;
            # contrast = 0.8916;
            # brightness = 0.8916;
            # vibrancy = 0.8916;
            # vibrancy_darkness = 0.0;
          }
        ];

        # Time display
        label = [
          {
            monitor = "";
            text = "cmd[update:1000] echo \"<span>$(date +\"%I:%M\")</span>\"";
            color = "rgba(216, 222, 233, 0.80)";
            font_size = 60;
            font_family = "CaskaydiaCove Nerd Font Bold";
            position = "30, -8";
            halign = "center";
            valign = "center";
          }
          # Date display
          {
            monitor = "";
            text = "cmd[update:1000] echo -e \"$(date +\"%A, %B %d\")\"";
            color = "rgba(216, 222, 233, 0.80)";
            font_size = 19;
            font_family = "CaskaydiaCove Nerd Font Bold";
            position = "35, -60";
            halign = "center";
            valign = "center";
          }
          # Username display
          {
            monitor = "";
            text = "    $USER";
            color = "rgba(216, 222, 233, 0.80)";
            font_size = 16;
            font_family = "CaskaydiaCove Nerd Font Bold";
            position = "38, -190";
            halign = "center";
            valign = "center";
          }
        ];

        # User box background
        shape = [
          {
            monitor = "";
            size = "320, 55";
            color = "rgba(255, 255, 255, 0.1)";
            rounding = -1;
            border_size = 0;
            position = "34, -190";
            halign = "center";
            valign = "center";
          }
        ];

        input-field = [
          {
            monitor = "";
            size = "320, 55";
            outline_thickness = 0;
            dots_size = 0.2;
            dots_spacing = 0.2;
            dots_center = true;
            outer_color = "rgba(255, 255, 255, 0)";
            inner_color = "rgba(255, 255, 255, 0.1)";
            font_color = "rgb(200, 200, 200)";
            fade_on_empty = false;
            font_family = "CaskaydiaCove Nerd Font Bold";
            placeholder_text = "<i><span foreground=\"##ffffff99\">🔒  Enter Password</span></i>";
            hide_input = false;
            position = "34, -268";
            halign = "center";
            valign = "center";
          }
        ];
      };
    };

    # Additional packages for Hyprland
    home.packages = with pkgs; [
      # Screen brightness
      brightnessctl

      # Media control
      playerctl

      # Clipboard manager
      cliphist
      wl-clipboard

      # Screenshots
      grim
      slurp

      # System monitoring
      lm_sensors
      pavucontrol

      # Additional utilities
      hyprpicker
      hyprcursor
      hyprpaper
      hyprlock

      # Fonts
      nerd-fonts.caskaydia-cove
    ];

    # Hypridle configuration
    services.hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = "pidof hyprlock || hyprlock";
          before_sleep_cmd = "loginctl lock-session";
          after_sleep_cmd = "hyprctl dispatch dpms on";
        };

        listener = [
          {
            timeout = 300;
            on-timeout = "loginctl lock-session";
          }
          {
            timeout = 330;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };

    services.hyprpolkitagent.enable = true;

    # Hyprpaper configuration
    services.hyprpaper = {
      enable = true;
      settings = {
        preload = [ ];
        wallpaper = [ ];
      };
    };
  };
}
