{
  lib,
  config,
  ...
}:
{
  # FIXME: Use sharedModules
  #imports = [ inputs.plasma-manager.homeManagerModules.plasma-manager ];

  programs.plasma = lib.mkIf config.programs.plasma.enable {
    overrideConfig = true;
    shortcuts = {
      "KDE Keyboard Layout Switcher"."Switch keyboard layout to English (US)" = [ ];
      "KDE Keyboard Layout Switcher"."Switch keyboard layout to Finnish" = [ ];
      "KDE Keyboard Layout Switcher"."Switch to Last-Used Keyboard Layout" = "Meta+Alt+L";
      "KDE Keyboard Layout Switcher"."Switch to Next Keyboard Layout" = [
        "Meta+Space"
        "Meta+Alt+K"
      ];
      "kmix"."decrease_microphone_volume" = "Microphone Volume Down";
      "kmix"."decrease_volume" = "Volume Down";
      "kmix"."decrease_volume_small" = "Shift+Volume Down";
      "kmix"."increase_microphone_volume" = "Microphone Volume Up";
      "kmix"."increase_volume" = "Volume Up";
      "kmix"."increase_volume_small" = "Shift+Volume Up";
      "kmix"."mic_mute" = [
        "Microphone Mute"
        "Meta+Volume Mute"
      ];
      "kmix"."mute" = "Volume Mute";
      "ksmserver"."Log Out" = "Ctrl+Alt+Del";
      "kwin"."Switch One Desktop Up" = "Meta+Ctrl+Up";
      "kwin"."Switch One Desktop to the Left" = "Meta+Ctrl+Left";
      "kwin"."Switch One Desktop to the Right" = "Meta+Ctrl+Right";
      "kwin"."Switch Window Down" = "Meta+Alt+Down";
      "kwin"."Switch Window Left" = "Meta+Alt+Left";
      "kwin"."Switch Window Right" = "Meta+Alt+Right";
      "kwin"."Switch Window Up" = "Meta+Alt+Up";
      "kwin"."Window Minimize" = "Meta+PgDown";
      "kwin"."Window Quick Tile Bottom" = "Meta+Down";
      "kwin"."Window Quick Tile Left" = "Meta+Left";
      "kwin"."Window Quick Tile Right" = "Meta+Right";
      "kwin"."Window Quick Tile Top" = "Meta+Up";
      "mediacontrol"."nextmedia" = "Media Next";
      "mediacontrol"."pausemedia" = "Media Pause";
      "mediacontrol"."playpausemedia" = "Media Play";
      "mediacontrol"."previousmedia" = "Media Previous";
      "mediacontrol"."stopmedia" = "Media Stop";
      "org_kde_powerdevil"."Decrease Keyboard Brightness" = "Keyboard Brightness Down";
      "org_kde_powerdevil"."Decrease Screen Brightness Small" = "Shift+Monitor Brightness Down";
      "org_kde_powerdevil"."Decrease Screen Brightness" = "Monitor Brightness Down";
      "org_kde_powerdevil"."Hibernate" = "Hibernate";
      "org_kde_powerdevil"."Increase Keyboard Brightness" = "Keyboard Brightness Up";
      "org_kde_powerdevil"."Increase Screen Brightness Small" = "Shift+Monitor Brightness Up";
      "org_kde_powerdevil"."Increase Screen Brightness" = "Monitor Brightness Up";
      "org_kde_powerdevil"."PowerDown" = "Power Down";
      "org_kde_powerdevil"."PowerOff" = "Power Off";
      "org_kde_powerdevil"."Sleep" = "Sleep";
      "org_kde_powerdevil"."Toggle Keyboard Backlight" = "Keyboard Light On/Off";
      "org_kde_powerdevil"."Turn Off Screen" = [ ];
      "org_kde_powerdevil"."powerProfile" = [
        "Battery"
        "Meta+B"
      ];
      "plasmashell"."activate application launcher" = [
        "Meta"
        "Alt+F1"
      ];
      "plasmashell"."activate task manager entry 1" = "Meta+1";
      "plasmashell"."activate task manager entry 2" = "Meta+2";
      "plasmashell"."activate task manager entry 3" = "Meta+3";
      "plasmashell"."activate task manager entry 4" = "Meta+4";
      "plasmashell"."activate task manager entry 5" = "Meta+5";
      "plasmashell"."activate task manager entry 6" = "Meta+6";
      "plasmashell"."activate task manager entry 7" = "Meta+7";
      "plasmashell"."activate task manager entry 8" = "Meta+8";
      "plasmashell"."activate task manager entry 9" = "Meta+9";
      "ksmserver"."Lock Session" = "Meta+L";
      "kwin"."Activate Window Demanding Attention" = "Meta+Ctrl+A";
      "kwin"."Kill Window" = "Meta+Ctrl+Esc";
      "kwin"."Overview" = "Meta+W";
      "kwin"."Switch One Desktop Down" = "Meta+Ctrl+Down";
      "kwin"."Walk Through Windows (Reverse)" = [
        "Meta+Shift+Tab"
        "Alt+Shift+Tab"
      ];
      "kwin"."Walk Through Windows" = [
        "Meta+Tab"
        "Alt+Tab"
      ];
      "kwin"."Window Close" = "Alt+F4";
      "kwin"."Window Maximize" = "Meta+PgUp";
    };
    input.keyboard = {
      options = [ "ctrl:swapcaps" ];
      layouts = [
        { layout = "us"; }
        { layout = "fi"; }
      ];
    };
    kwin = {
      nightLight = {
        enable = true;
        mode = "times";
        temperature = {
          night = 4600;
        };
        time = {
          morning = "06:00";
          evening = "17:00";
        };
        transitionTime = 30;
      };
    };
    session.sessionRestore.restoreOpenApplicationsOnLogin = "startWithEmptySession";
    configFile = {
      "kwinrc"."MouseBindings"."CommandTitlebarWheel" = "Change Opacity";
      "kwinrc"."Windows"."Placement" = "Smart";
      "kwinrc"."Plugins"."hidecursorEnabled" = true;
      "kwinrc"."Plugins"."zoomEnabled" = false;
      "kwinrc"."Effect-overview"."BorderActivate" = 9;
      "kwinrc"."Plugins"."diminactiveEnabled" = true;
      "kwinrc"."Effect-diminactive"."DimDesktop" = true;
      "kwinrc"."Plugins"."fallapartEnabled" = false;
      "kwinrc"."TabBox"."HighlightWindows" = false;
      "kwinrc"."Plugins"."sheetEnabled" = false;
      "kwinrc"."Plugins"."wobblywindowsEnabled" = false;

      "dolphinrc"."VersionControl"."enabledPlugins" = "Git";
      "kxkbrc"."Layout"."Options" = "ctrl:swapcaps";
      "kwinrc"."ElectricBorders"."TopLeft" = null;
      "krunnerrc"."General"."FreeFloating" = true;
      "krunnerrc"."General"."historyBehavior" = "ImmediateCompletion";
      "kwinrc"."Input"."TabletMode" = "off";
    };
    panels = [
      {
        height = 40;
        lengthMode = "fit";
        location = "right";
        alignment = "center";
        hiding = "dodgewindows";
        floating = true;
        widgets = [
          "org.kde.plasma.kickoff"
          "org.kde.plasma.pager"
          "org.kde.plasma.icontasks"
          "org.kde.plasma.marginsseparator"
          "org.kde.plasma.systemtray"
          "org.kde.plasma.digitalclock"
        ];
      }

    ];
  };

}
