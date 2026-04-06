{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the paths it should
  # manage.
  home.username = "wharding";
  home.homeDirectory = "/home/wharding";

  # This value determines the Home Manager release that your configuration is
  # compatible with. This helps avoid breakage when a new Home Manager release
  # introduces backwards incompatible changes.
  #
  # You should not change this value, even if you update Home Manager. If you do
  # want to update the value, then make sure to first check the Home Manager
  # release notes.
  home.stateVersion = "25.11"; # Please read the comment before changing.

  # The home.packages option allows you to install Nix packages into your
  # environment.
  home.packages = with pkgs; [
    # # It is sometimes useful to fine-tune packages, for example, by applying
    # # overrides. You can do that directly here, just don't forget the
    # # parentheses. Maybe you want to install Nerd Fonts with a limited number of
    # # fonts?
    # (pkgs.nerdfonts.override { fonts = [ "FantasqueSansMono" ]; })

    # # You can also create simple shell scripts directly inside your
    # # configuration. For example, this adds a command 'my-hello' to your
    # # environment:
    # (pkgs.writeShellScriptBin "my-hello" ''
    #   echo "Hello, ${config.home.username}!"
    # '')

    #rustfmt
    #rustup

    _1password-gui
    _1password-cli
    calibre
    claude-code
    clojure
    dune
    joplin-desktop
    #karere # whatsapp client
    lite
    maestral
    mate-calc
    meld
    ocaml
    rawtherapee
    #signal-desktop
    tmux
    trayscale
    zeal
    isync
  ];

  programs.joplin-desktop.enable = true;
  #programs.offlineimap.enable = true;
  programs.emacs.enable = true;
  programs.tmux.enable = true;
  programs.git.enable = true;
  programs.vim.enable = true;
  programs.git.package = pkgs.gitFull;
  programs.freetube.enable = true;

  
  # Home Manager is pretty good at managing dotfiles. The primary way to manage
  # plain files is through 'home.file'.
  home.file = {
    # # Building this configuration will create a copy of 'dotfiles/screenrc' in
    # # the Nix store. Activating the configuration will then make '~/.screenrc' a
    # # symlink to the Nix store copy.
    # ".screenrc".source = dotfiles/screenrc;
    ".bashrc".source = dots/bashrc;

    # # You can also set the file content immediately.
    # ".gradle/gradle.properties".text = ''
    #   org.gradle.console=verbose
    #   org.gradle.daemon.idletimeout=3600000
    # '';
  };
  programs.vim = {
    extraConfig = ''
      set laststatus=2
      nnoremap <C-h> <C-w>h
      nnoremap <C-j> <C-w>j
      nnoremap <C-k> <C-w>k
      nnoremap <C-l> <C-w>l
      tnoremap <C-h> <C-w>h
      tnoremap <C-j> <C-w>j
      tnoremap <C-k> <C-w>k
      tnoremap <C-l> <C-w>l
      "source /usr/share/doc/fzf/examples/plugin/fzf.vim 
      nnoremap <C-p> :FZF<CR>
    '';
    plugins = [ pkgs.vimPlugins.fzf-vim ];
  };

  programs.joplin-desktop.sync.target = "dropbox";
  programs.git.settings = {
    user = {
      name = "Will Harding";
      email = "harding.will@gmail.com";
    };
    alias = {
       st = "status";
       ci = "commit";
       d = "diff";
       co = "checkout";
       llog = "log --decorate --graph --branches";
    };
    push = { default = "simple"; };
    core = {
      excludesfile = "~/.gitignore_global";
      quotepath = false;
    };
    merge = { tool = "tkdiff"; };
    #
    # [includeIf "gitdir:~/work/"]
    #   path = ~/work/.gitconfig
    #
    diff = { algorithm = "patience"; };
    init = { defaultBranch = "main"; };
  };
  programs.vscode.enable = true;
  programs.vscode.mutableExtensionsDir = true; # TODO fix this
  programs.vscode.package = pkgs.vscodium;
  programs.keepassxc.enable = true;

  programs.emacs.extraPackages = epkgs: with epkgs; [
    # current-window-only
    add-node-modules-path
    cider
    clojure-mode
    dash
    dumb-jump
    eglot
    elfeed
    emacs
    evil
    evil-collection
    exec-path-from-shell
    go-mode
    inf-clojure
    lua-mode
    magit
    marginalia
    markdown-mode
    nix-mode
    nodejs-repl
    notmuch
    ob-http
    olivetti
    orderless
    org
    org-ql
    prettier-js
    project
    #rcirc
    rc-mode
    reason-mode
    restclient
    rg
    rust-mode
    scala-mode
    #scheme-mode
    sql-indent
    treemacs
    tuareg
    typescript-mode
    undo-tree
    vertico
    web-mode
    yaml-mode
    kubel
  ];
  # Home Manager can also manage your environment variables through
  # 'home.sessionVariables'. These will be explicitly sourced when using a
  # shell provided by Home Manager. If you don't want to manage your shell
  # through Home Manager then you have to manually source 'hm-session-vars.sh'
  # located at either
  #
  #  ~/.nix-profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  ~/.local/state/nix/profiles/profile/etc/profile.d/hm-session-vars.sh
  #
  # or
  #
  #  /etc/profiles/per-user/wharding/etc/profile.d/hm-session-vars.sh
  #
  home.sessionVariables = {
    # EDITOR = "emacs";
  };

  programs.notmuch.enable = true;
  programs.mbsync.enable = true;
  programs.lieer.enable = true;
  # TODO: Move the mail
  # accounts.email.maildirBasePath = "Mail";
  # TODO: Need to enable near creation for MBsync stuff "-Cn / --create-near" on the 
  accounts.email.accounts.Wnhx = {
    enable = true;
    primary = true;
    address = "will@wnhx.ca";
    realName = "Will Harding";
    flavor = "migadu.com";
    notmuch.enable = true;
    #offlineimap.enable = true;
    mbsync.enable = true;
    msmtp.enable = true;
    passwordCommand = "cat /home/wharding/_secrets/Wnhx";
    imap = {
      host = "imap.migadu.com";
      tls.enable = true;
    };
  };
  accounts.email.accounts.Gmail = {
    enable = true;
    address = "harding.will@gmail.com";
    realName = "Will Harding";
    flavor = "gmail.com";
    notmuch.enable = true;
    #offlineimap.enable = true;
    mbsync.enable = true;
    #lieer.enable = true;
    passwordCommand = "cat /home/wharding/_secrets/Gmail";
    msmtp.enable = true;
    imap = {
      host = "imap.gmail.com";
      tls.enable = true;
    };
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
