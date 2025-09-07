# Desktop Environment Modules

This directory contains NixOS modules for different desktop environments.

## Available Desktop Environments

### KDE Plasma 6 (Default)

- **Module**: `plasma.nix`
- **Status**: Enabled by default
- **Display Manager**: SDDM with Wayland
- **Features**: Full KDE Plasma 6 desktop environment with custom shortcuts and configuration

### Hyprland

- **Module**: `hyprland.nix`
- **Status**: Available but disabled by default
- **Display Manager**: SDDM with Wayland
- **Features**: Tiling window manager with Waybar, Mako notifications, and Hyprlock

## Switching to Hyprland

To enable Hyprland instead of or alongside Plasma, add this to your machine configuration:

```nix
{
  # Enable Hyprland
  programs.hyprland.enable = true;

  # Optionally disable Plasma if you only want Hyprland
  # services.desktopManager.plasma6.enable = false;
}
```

## Key Bindings (Hyprland)

The Hyprland configuration maintains compatibility with Plasma shortcuts where possible:

### Window Management

- `Super + Arrow Keys`: Move windows
- `Super + Alt + Arrow Keys`: Focus windows
- `Super + Page Up/Down`: Maximize/Minimize
- `Super + L`: Lock screen
- `Alt + Tab`: Window switching

### Application Shortcuts

- `Super + Space`: Application launcher (rofi)
- `Super + Return`: Terminal (kitty)
- `Super + E`: File manager (dolphin)
- `Super + W`: Window overview
- `Super + 1-9`: Switch workspaces

### System Controls

- `Super + Ctrl + Arrow Keys`: Switch desktops
- Volume/Brightness keys work as expected
- Media keys (play/pause/next/previous)

## Included Applications

Both desktop environments include:

- File manager (Dolphin)
- System utilities (Filelight, Partition Manager, System Log)
- Calculator, image scanning tools
- Development tools and productivity applications

Hyprland additionally includes:

- Waybar (status bar)
- Mako (notifications)
- Rofi (application launcher)
- Screenshot tools (grim/slurp)
- Hyprlock (screen locker)

## Configuration Files

- **Plasma**: Configuration managed through `plasma-manager`
- **Hyprland**: Configuration in `modules/home/hyprland.nix`

Both can be customized per-user through Home Manager modules.
