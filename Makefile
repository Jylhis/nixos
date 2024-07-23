
build:
	nixos-rebuild --flake ~/nixos-config build

switch:
	nixos-rebuild --flake ~/nixos-config --use-remote-sudo switch

symlink:
	sudo mv /etc/nixos /etc/nixos.bak  # Backup the original configuration
	sudo ln -s ~/nixos-config/ /etc/nixos
