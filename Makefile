
build:
	nixos-rebuild --flake ~/nixos-config build

switch:
	nixos-rebuild --flake ~/nixos-config --use-remote-sudo switch

clean:
	nix store gc; nix-collect-garbage -d ; nix store optimise

symlink:
	sudo mv /etc/nixos /etc/nixos.bak  # Backup the original configuration
	sudo ln -s ~/nixos-config/ /etc/nixos
