# Desktop NixOS configuration module.
#
# This module provides a complete desktop environment configuration
# suitable for workstations and personal computers. It includes
# graphical desktop environment, fonts, and peripheral support.
{
  imports = [
    ./common.nix
    ./system.nix
    ./plasma.nix
    ./gnome.nix
    ./hyprland.nix
    ./fonts.nix
    ../_1password.nix
    ../logitech.nix
    ../users.nix
    ../plymouth.nix
  ];
}
