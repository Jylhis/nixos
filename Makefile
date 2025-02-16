ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

build:
	nixos-rebuild --flake $(ROOT_DIR) build

switch:
	nixos-rebuild --flake $(ROOT_DIR) --use-remote-sudo switch |& nom

switch-plain:
	nixos-rebuild --flake $(ROOT_DIR) --use-remote-sudo switch

server-vm:
	nixos-rebuild switch --use-remote-sudo --flake "$(ROOT_DIR)#server" --target-host root@192.168.1.53

push-cache:
	nix path-info --all | cachix push jylhis-nixos

clean:
	nix store gc; nix-collect-garbage -d ; nix store optimise

test:
	nix run github:nix-community/nixos-anywhere -- --flake $(ROOT_DIR)#server --vm-test
