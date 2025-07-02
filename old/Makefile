SHELL=/usr/bin/env bash
.PHONY: sign-store build switch boot clean cachix cache cache-emacs bootstrap

ROOT_DIR:=$(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
CACHE_URL:="ssh://lab"
TARGET:=$(shell hostname)
FULL_TARGET:=$(ROOT_DIR)\#$(TARGET)



# Daily stuff

build:
	nh os build $(FULL_TARGET)

switch:
	nh os $(FULL_TARGET) switch --ask

boot:
	nh os $(FULL_TARGET) boot --ask

#clean:
	#nh clean all --keep-since 30d --keep 3


# Cache
cachix:
	nix path-info --all | cachix push jylhis-nixos

push-cache-private-all: sign-store
	nix copy --to "$(CACHE_URL)" $(nix path-info --all)

cache: sign-store
	@echo "Push inputs"
	nix flake archive --json | jq -r '.path,(.inputs|to_entries[].value.path)' | nix copy --to "$(CACHE_URL)" --stdin

cache-emacs:
	nix build .#emacs-markus --no-link --print-out-paths | nix copy --to "$(CACHE_URL)" --stdin

sign-store:
	nix store sign --key-file $(ROOT_DIR)/secrets/cache-priv-key.pem --all


# Development

test:
	nix run github:nix-community/nixos-anywhere -- --flake $(ROOT_DIR)#server --vm-test


# Bootstrap stuff
bootstrap:
	nixos-rebuild --flake $(FULL_TARGET) --fast build
