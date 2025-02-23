ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))

build:
	nixos-rebuild --flake $(ROOT_DIR) build

switch-mow:
	nixos-rebuild --flake $(ROOT_DIR) --use-remote-sudo switch |& nom

switch:
	nixos-rebuild --flake $(ROOT_DIR) --use-remote-sudo switch

boot:
	nixos-rebuild --flake $(ROOT_DIR) --use-remote-sudo boot

server-vm:
	nixos-rebuild switch --use-remote-sudo --flake "$(ROOT_DIR)#server" --target-host root@192.168.1.53

push-cachix:
	nix path-info --all | cachix push jylhis-nixos

push-cache-private: sign-store
	nix copy --to "ssh://lab" $(nix path-info --all)

sign-store:
	nix store sign --key-file $(ROOT_DIR)/secrets/cache-priv-key.pem --all

clean:
	nix store gc; nix-collect-garbage -d ; nix store optimise

test:
	nix run github:nix-community/nixos-anywhere -- --flake $(ROOT_DIR)#server --vm-test
