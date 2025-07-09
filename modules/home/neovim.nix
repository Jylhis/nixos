{ pkgs, ... }:
{

  programs.neovim = {
    enable = true;
    plugins = with pkgs.vimPlugins; [
      vim-nix
      vim-lastplace
    ];
    extraPackages = [
      #pkgs.vim-language-server
      pkgs.xclip # X11
      # pkgs.wl-copy # Wayland
    ];
    defaultEditor = false;
    viAlias = true;
    vimAlias = true;
    extraConfig = ''
                " your custom vimrc
                set nocompatible
                set backspace=indent,eol,start
                " Turn on syntax highlighting by default
                syntax on
      	  set number
                set cursorline
      	  set diffopt+=iwhite
                set wildignorecase
      	  set copyindent
                set smarttab
      	  set autoindent
                set smartindent

                set hlsearch "when there is a previous search pattern, highlight all its matches
                set incsearch "while typing a search command, show immediately where the so far typed pattern matches
                set ignorecase "ignore case in search patterns
                set smartcase "override the 'ignorecase' option if the search pattern contains uppercase characters
                set gdefault "imply global for new searches

                " Stuff


                " disable vi compatibility
                set nocompatible

                set encoding=utf-8
      	  filetype plugin indent on
    '';
  };

}
