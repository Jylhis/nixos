(

 (nix-ts-mode
  . ((eglot-workspace-configuration
      . (
	 :nil
                 (:formatting
                  (:command ["nixfmt"])
                  :nix (:flake (:autoArchive t :autoEvalInputs t))
		  )

	 :nixd
	  (:nixpkgs (:expr "import <nixpkgs> { }")
		    :options (:nixos (:expr "(builtins.getFlake \"/etc/nixos\").nixosConfigurations.mac-mini.options")
				     :nixos-server (:expr "(builtins.getFlake \"/etc/nixos\").nixosConfigurations.server.options")
				     :home-manager (:expr "(builtins.getFlake \"/etc/nixos\").nixosConfigurations.mac-mini.options.home-manager.users.type.getSubOptions []")


	    ))

	 )
      ))
 )

 (emacs-lisp-mode . ((lexical-binding . t)))
 )
