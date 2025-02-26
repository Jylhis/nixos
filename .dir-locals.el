(

 (nix-ts-mode
  . ((eglot-workspace-configuration
      . (:nixd
	  (:nixpkgs (:expr "import <nixpkgs> { }")
		    :options (:nixos (:expr "(builtins.getFlake \"/etc/nixos\").nixosConfigurations.mac-mini.options")
				     :nixos-server (:expr "(builtins.getFlake \"/etc/nixos\").nixosConfigurations.server.options")

	    ))

	 )
      ))
 )

 (emacs-lisp-mode . ((lexical-binding . t)))
 )
