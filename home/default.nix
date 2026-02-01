# Home Manager configuration for ericbarbour
{
  config,
  pkgs,
  lib,
  inputs,
  username,
  ...
}:

let
  # ===== Zsh Theme Selection =====
  # Options: "spaceship", "powerlevel10k", "starship"
  # Starship is the fastest (Rust-based, ~10ms vs 100-200ms for spaceship)
  zshTheme = "starship";
in
{
  home = {
    username = username;
    homeDirectory = "/Users/${username}";
    stateVersion = "24.05";

    # Packages installed to user profile
    packages = with pkgs; [
      # ===== Modern CLI Replacements =====
      # These replace/improve standard Unix tools

      ripgrep # grep replacement (rg)
      silver-searcher # ag - fast code search
      fd # find replacement
      sd # sed replacement - simpler syntax: sd 'find' 'replace'
      procs # ps replacement - colorful, human-readable
      dust # du replacement - visual disk usage
      duf # df replacement - disk free with colors
      bottom # htop replacement - graphs, GPU, temp (btm)
      tealdeer # tldr - simplified man pages with examples

      # ===== File & Text Tools =====
      bat # cat replacement - syntax highlighting
      eza # ls replacement - icons, git status
      jq # JSON processor
      yq # YAML processor
      fzf # Fuzzy finder
      tree # Directory tree

      # ===== Navigation & History =====
      zoxide # Smart cd - learns your habits
      # atuin configured below via programs.atuin

      # ===== Git Tools =====
      lazygit # TUI git client
      tig # Text-mode interface for git
      delta # Better git diffs
      difftastic # Structural diff - understands code syntax
      gh # GitHub CLI

      # ===== Network Tools =====
      gping # Graphical ping
      xh # HTTPie alternative - curl for humans
      bandwhich # Bandwidth monitor by process
      dog # DNS client (dig replacement)

      # ===== Dev Tools =====
      just # Task runner (better make)
      watchexec # File watcher - run commands on changes
      hyperfine # Benchmarking tool
      tokei # Code statistics

      # ===== System Tools =====
      htop # Fallback process viewer
      ncdu # NCurses disk usage
      pstree # Process tree

      # ===== Additional Modern Tools =====
      yazi # Terminal file manager (blazing fast, Rust)
      zellij # Terminal multiplexer (modern tmux)
      tmux # Classic terminal multiplexer
      glow # Markdown renderer in terminal
      fx # Interactive JSON viewer
      ouch # Universal archive tool (tar/zip/7z replacement)
      viddy # Modern watch command with diff highlighting
      grex # Regex generator from examples
      mtr # Network diagnostics (ping + traceroute)
      age # Simple, modern file encryption
      broot # Interactive directory navigator

      # ===== Shell Prompt & System Info =====
      spaceship-prompt # Zsh prompt for astronauts
      zsh-powerlevel10k # Powerlevel10k theme
      starship # Cross-shell prompt (blazing fast, Rust)
      nushell # Modern shell with structured data pipelines
      fastfetch # System info display (fast neofetch replacement)
      macchina # Minimal system info fetcher

      # ===== Container & Kubernetes =====
      lazydocker # TUI for Docker management
      k9s # TUI for Kubernetes clusters
      dive # Analyze Docker image layers
      ctop # Top-like container metrics

      # ===== Database Tools =====
      usql # Universal SQL client (Postgres, MySQL, SQLite, etc.)
      pgcli # Smart Postgres CLI with autocomplete
      litecli # SQLite CLI with autocomplete

      # ===== API & HTTP Tools =====
      hurl # HTTP request testing with assertions
      posting # TUI API client (Postman alternative)

      # ===== Security Scanning =====
      trivy # Vulnerability scanner for containers/repos
      gitleaks # Find secrets in git history
      trufflehog # Deep secret scanning with verification

      # ===== Data Processing =====
      miller # CSV/JSON Swiss army knife (mlr)
      jnv # Interactive jq with live preview
      dasel # Query JSON/YAML/TOML/XML
      gron # Make JSON greppable

      # ===== File Sync & Transfer =====
      rclone # Rsync for cloud storage (S3, GDrive, Dropbox)

      # ===== System Monitoring =====
      zenith # System monitor with zoomable charts
      glances # Cross-platform system monitor
      sampler # Custom CLI dashboards from YAML

      # ===== Demo & Documentation Tools =====
      vhs # Create terminal GIFs as code
      gum # Glamorous shell script prompts
      charm-freeze # Generate images of code/terminal output
      slides # Terminal-based presentations

      # ===== AI Tools =====
      aichat # Multi-model AI CLI (20+ providers)
      crush # Charmbracelet AI coding agent (glamorous terminal TUI)
      playwright-mcp # Playwright MCP server for browser automation

      # ===== Nix Tools =====
      nil # Nix LSP
      nixfmt-rfc-style
      nix-tree # Visualize nix dependencies

      # ===== Shell Enhancements =====
      vivid # LS_COLORS generator
      carapace # Multi-shell completion generator
      zsh-completions

      # ===== Neovim =====
      neovim

      # ===== LSPs (for Neovim/Zed) =====
      lua-language-server
      stylua
      typescript-language-server
      nodePackages.prettier
      vscode-langservers-extracted # HTML, CSS, JSON, ESLint
      yaml-language-server
      taplo # TOML
      marksman # Markdown
      pyright
      ruff
      gopls
      rust-analyzer
      shellcheck
      shfmt

      # ===== Neovim Dependencies =====
      gcc # Required for treesitter
      gnumake # Required for avante.nvim
      cargo # For building rust plugins

      # ===== Languages & Runtimes =====
      nodejs_22 # LTS - needed for Copilot, LSPs
      # bun
      # deno
      # rustup
    ];

    # Session variables
    sessionVariables = {
      EDITOR = "nvim";
      VISUAL = "nvim";
      MANPAGER = "sh -c 'col -bx | bat -l man -p'";
      MANROFFOPT = "-c";
    };
  };

  # Config file symlinks
  xdg.configFile = {
    # Zed
    "zed/settings.json".source = ../config/zed/settings.json;
    "zed/keymap.json".source = ../config/zed/keymap.json;
    "zed/tasks.json".source = ../config/zed/tasks.json;

    # Neovim
    "nvim/init.lua".source = ../config/nvim/init.lua;

    # Starship prompt
    "starship.toml".source = ../config/starship.toml;

    # Ghostty terminal
    "ghostty/config".source = ../config/ghostty/config;

    # Lazygit
    "lazygit/config.yml".source = ../config/lazygit/config.yml;

    # Yazi file manager
    "yazi/yazi.toml".source = ../config/yazi/yazi.toml;
    "yazi/keymap.toml".source = ../config/yazi/keymap.toml;
  };

  # Home directory files (not in .config)
  home.file = {
    # AeroSpace (expects ~/.aerospace.toml)
    ".aerospace.toml".source = ../config/aerospace.toml;
  };

  # Let home-manager manage itself
  programs.home-manager.enable = true;

  # ===== Atuin - Better Shell History =====
  # Syncs history across machines, powerful search
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
    flags = [ "--disable-up-arrow" ]; # Keep up-arrow for normal history
    settings = {
      auto_sync = false; # Set true + login to sync across machines
      sync_frequency = "5m";
      search_mode = "fuzzy";
      filter_mode = "global";
      style = "compact";
      inline_height = 20;
      show_preview = true;
      # Ctrl+R opens atuin search
    };
  };

  # Git configuration
  programs.git = {
    enable = true;
    settings = {
      user = {
        name = "Eric Barbour";
        email = "barbour.ericm@gmail.com";
      };
      init.defaultBranch = "main";
      pull.rebase = true;
      push.autoSetupRemote = true;
      core.editor = "vim";
      merge.conflictstyle = "diff3";
      diff = {
        colorMoved = "default";
        # Use difftastic for structural diffs (opt-in)
        # external = "difft";  # Uncomment to make default
      };
      alias = {
        st = "status";
        co = "checkout";
        br = "branch";
        ci = "commit";
        lg = "log --oneline --graph --decorate";
        # Difftastic aliases
        dft = "difftool";
        dlog = "!f() { GIT_EXTERNAL_DIFF=difft git log -p --ext-diff $@; }; f";
      };
      difftool = {
        prompt = false;
        difftastic.cmd = ''difft "$LOCAL" "$REMOTE"'';
      };
    };
  };

  # Delta (better git diffs)
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      navigate = true;
      line-numbers = true;
      syntax-theme = "Dracula";
      side-by-side = false; # Set true for side-by-side view
      file-style = "bold yellow ul";
      hunk-header-style = "omit";
    };
  };

  # ===== Zsh Configuration =====
  programs.zsh = {
    enable = true;
    autosuggestion.enable = true;
    syntaxHighlighting.enable = true;
    enableCompletion = true;

    # Completion styling
    completionInit = ''
      # Case-insensitive completion
      zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}'
      # Menu selection
      zstyle ':completion:*' menu select
      # Colors in completion
      zstyle ':completion:*' list-colors ''${(s.:.)LS_COLORS}
      # Descriptions
      zstyle ':completion:*:descriptions' format '%F{yellow}-- %d --%f'
      zstyle ':completion:*:warnings' format '%F{red}-- no matches --%f'
      # Group completions
      zstyle ':completion:*' group-name '''
    '';

    history = {
      size = 100000;
      save = 100000;
      ignoreDups = true;
      ignoreAllDups = true;
      ignoreSpace = true;
      extended = true;
      share = true;
      expireDuplicatesFirst = true;
    };

    # Oh My Zsh integration (minimal plugins for speed)
    # docker/kubectl completions handled by carapace instead
    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "sudo" # ESC ESC to prepend sudo
        "extract" # Universal archive extractor
      ];
    };

    initContent = lib.mkMerge [
      # Must be at top - Powerlevel10k instant prompt (only when using p10k)
      (lib.mkBefore (
        lib.optionalString (zshTheme == "powerlevel10k") ''
          if [[ -r "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh" ]]; then
            source "''${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-''${(%):-%n}.zsh"
          fi
        ''
      ))

      # Regular init content
      ''
        # ===== Theme Configuration =====
        ${
          if zshTheme == "spaceship" then
            ''
              # Spaceship prompt
              source ${pkgs.spaceship-prompt}/share/zsh/themes/spaceship.zsh-theme
              # Spaceship options (customize as needed)
              # See: https://spaceship-prompt.sh/options/
              SPACESHIP_PROMPT_ADD_NEWLINE=true
              SPACESHIP_PROMPT_SEPARATE_LINE=true
              SPACESHIP_CHAR_SYMBOL="❯ "
              SPACESHIP_DIR_TRUNC=3
            ''
          else if zshTheme == "powerlevel10k" then
            ''
              # Powerlevel10k theme
              source ${pkgs.zsh-powerlevel10k}/share/zsh-powerlevel10k/powerlevel10k.zsh-theme
              [[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
            ''
          else if zshTheme == "starship" then
            ''
              # Starship prompt (already installed, just needs init)
              eval "$(starship init zsh)"
            ''
          else
            ''
              # No theme configured
            ''
        }

        # Load secrets (API keys, tokens - never commit)
        [[ -f ~/.secrets ]] && source ~/.secrets

        # LS_COLORS with vivid (cached for speed)
        _vivid_cache="$HOME/.cache/vivid-ls-colors"
        if [[ ! -f "$_vivid_cache" ]] || [[ $(find "$_vivid_cache" -mtime +7 2>/dev/null) ]]; then
          mkdir -p "$(dirname "$_vivid_cache")"
          vivid generate dracula > "$_vivid_cache"
        fi
        export LS_COLORS="$(<$_vivid_cache)"

        # Carapace completions (cached for speed)
        _carapace_cache="$HOME/.cache/carapace-init.zsh"
        if [[ ! -f "$_carapace_cache" ]] || [[ $(find "$_carapace_cache" -mtime +7 2>/dev/null) ]]; then
          mkdir -p "$(dirname "$_carapace_cache")"
          carapace _carapace > "$_carapace_cache"
        fi
        export CARAPACE_BRIDGES='zsh,fish,bash,inshellisense'
        source "$_carapace_cache"

        # NVM (lazy-loaded for fast shell startup)
        export NVM_DIR="$HOME/.nvm"
        _nvm_lazy_load() {
          unset -f nvm node npm npx
          [ -s "$NVM_DIR/nvm.sh" ] && source "$NVM_DIR/nvm.sh"
        }
        nvm() { _nvm_lazy_load; nvm "$@"; }
        node() { _nvm_lazy_load; node "$@"; }
        npm() { _nvm_lazy_load; npm "$@"; }
        npx() { _nvm_lazy_load; npx "$@"; }

        # Bun (lazy-loaded PATH only, skip completions for speed)
        export BUN_INSTALL="$HOME/.bun"
        export PATH="$BUN_INSTALL/bin:$PATH"

        # OpenCode
        export PATH=$HOME/.opencode/bin:$PATH

        # Amp Code
        export PATH=$HOME/.amp/bin:$PATH

        # Native CLI tools (Claude Code, etc.)
        export PATH=$HOME/.local/bin:$PATH

        # Yazi: cd to directory on exit
        function ya() {
          local tmp="$(mktemp -t "yazi-cwd.XXXXX")"
          yazi "$@" --cwd-file="$tmp"
          if cwd="$(cat -- "$tmp")" && [ -n "$cwd" ] && [ "$cwd" != "$PWD" ]; then
            cd -- "$cwd"
          fi
          rm -f -- "$tmp"
        }

        # Better keybindings
        bindkey '^[[A' history-search-backward  # Up arrow
        bindkey '^[[B' history-search-forward   # Down arrow
        bindkey '^[^[[D' backward-word          # Alt+Left
        bindkey '^[^[[C' forward-word           # Alt+Right
      ''
    ];

    shellAliases = {
      # === Nix ===
      rebuild = "darwin-rebuild switch --flake ~/dotfiles";

      # === Modern Replacements ===
      # eza aliases handled by programs.eza with enableZshIntegration
      cat = "bat";
      grep = "rg";
      find = "fd";
      sed = "sd";
      ps = "procs";
      du = "dust";
      df = "duf";
      top = "btm";
      htop = "btm";
      ping = "gping";
      dig = "dog";
      curl = "xh";
      man = "tldr"; # Quick reference; use `command man` for full man

      # === Git ===
      g = "git";
      gs = "git status";
      gd = "git diff";
      gds = "git diff --staged";
      gc = "git commit";
      gca = "git commit --amend";
      gp = "git push";
      gl = "git pull";
      gco = "git checkout";
      gb = "git branch";
      glog = "git log --oneline --graph --decorate -20";
      gdft = "git difftool"; # Difftastic

      # === Tools ===
      zj = "zellij";
      lg = "lazygit";
      j = "just";
      v = "nvim";
      vi = "nvim";
      vim = "nvim";

      # === Navigation ===
      ".." = "cd ..";
      "..." = "cd ../..";
      "...." = "cd ../../..";

      # === Safety ===
      rm = "rm -i";
      cp = "cp -i";
      mv = "mv -i";

      # === Misc ===
      ports = "lsof -i -P -n | grep LISTEN";
      myip = "curl -s ifconfig.me";
      weather = "curl -s wttr.in";
      path = "echo $PATH | tr ':' '\n'";
    };
  };

  # FZF - fuzzy finder
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultCommand = "fd --type f --hidden --exclude .git";
    defaultOptions = [
      "--height=40%"
      "--layout=reverse"
      "--border"
      "--preview 'bat --color=always --style=numbers --line-range=:500 {}'"
    ];
    colors = {
      # Dracula theme
      fg = "#f8f8f2";
      bg = "#282a36";
      hl = "#bd93f9";
      "fg+" = "#f8f8f2";
      "bg+" = "#44475a";
      "hl+" = "#bd93f9";
      info = "#ffb86c";
      prompt = "#50fa7b";
      pointer = "#ff79c6";
      marker = "#ff79c6";
      spinner = "#ffb86c";
      header = "#6272a4";
    };
  };

  # Zoxide (smart cd)
  programs.zoxide = {
    enable = true;
    enableZshIntegration = true;
    options = [ "--cmd cd" ]; # Replace cd entirely
  };

  # Bat (better cat)
  programs.bat = {
    enable = true;
    config = {
      theme = "Dracula";
      italic-text = "always";
      pager = "less -FR";
    };
    extraPackages = with pkgs.bat-extras; [
      batdiff # diff with bat
      batgrep # grep with bat
      batman # man with bat
      prettybat # format and bat
    ];
  };

  # Direnv for per-project environments
  programs.direnv = {
    enable = true;
    enableZshIntegration = true;
    nix-direnv.enable = true;
  };

  # Eza (ls replacement)
  programs.eza = {
    enable = true;
    icons = "always";
    git = true;
    extraOptions = [
      "--group-directories-first"
      "--header"
    ];
    enableZshIntegration = true;
  };

  # Bottom (system monitor)
  programs.bottom = {
    enable = true;
    settings = {
      flags = {
        color = "gruvbox";
        tree = true;
        battery = true;
      };
    };
  };
}
