ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

build:
	nixos-rebuild --flake $(ROOT_DIR) build

switch:
	nixos-rebuild --flake $(ROOT_DIR) --use-remote-sudo switch

clean:
	nix store gc; nix-collect-garbage -d ; nix store optimise

#symlink:
	#sudo mv /etc/nixos /etc/nixos.bak  # Backup the original configuration
	#sudo ln -s ~/nixos-config/ /etc/nixos
