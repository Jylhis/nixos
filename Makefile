
build:
	nixos-rebuild --flake /etc/nixos build

switch:
	nixos-rebuild --flake /etc/nixos --use-remote-sudo switch

clean:
	nix store gc; nix-collect-garbage -d ; nix store optimise

#symlink:
	#sudo mv /etc/nixos /etc/nixos.bak  # Backup the original configuration
	#sudo ln -s ~/nixos-config/ /etc/nixos
