# Desktop NixOS configuration module.
#
# This module provides a complete desktop environment configuration
# suitable for workstations and personal computers. It includes
# graphical desktop environment, fonts, and peripheral support.
{
  imports = [
    ./common.nix
    ./gnome.nix
    ./hyprland.nix
    ../logitech.nix
    ../users.nix
  ];
}
