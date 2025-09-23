;;; -*- lexical-binding: t -*-
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((claude-code-ide :url "https://github.com/manzaltu/claude-code-ide.el")))
 '(safe-local-variable-values
   '((compile-multi-profiles
      '(("NixOS Rebuild" . "sudo nixos-rebuild switch --flake .#desktop")
	("Bank Importer" . "cd sources/bank-importer && cabal build")
	("Learning Haskell" . "cd sources/learning-haskell && cabal build")
	("Repo Indexer" . "cd sources/repo_indexer && just build")
	("Update flake.lock" . "nix flake update"))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
