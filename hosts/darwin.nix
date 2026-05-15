# System-level macOS configuration
{
  pkgs,
  inputs,
  username,
  hostname,
  ...
}:

{
  # Required for nix-darwin
  nixpkgs.hostPlatform = "aarch64-darwin";

  # Primary user (required for user-specific settings like dock, finder, homebrew)
  system.primaryUser = username;

  # Nix settings - disable nix-darwin management since we use Determinate installer
  nix.enable = false;

  # System packages available to all users
  environment.systemPackages = with pkgs; [
    vim
    git
    curl
    wget
  ];

  # Enable zsh (required for proper PATH integration)
  programs.zsh.enable = true;

  # Touch ID for sudo (new syntax)
  security.pam.services.sudo_local.touchIdAuth = true;

  # Homebrew packages (managed declaratively)
  # Your existing brew packages go here - migrate gradually
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap"; # Remove unlisted packages
      upgrade = true;
    };

    taps = [
      # homebrew/services is deprecated and now built-in
      "nikitabobko/tap" # AeroSpace tiling window manager
      "FelixKratz/formulae" # SketchyBar (optional status bar)
    ];

    # CLI tools - migrate your brew formulas here over time
    brews = [
      # Essential tools you currently have via brew
      "zsh-syntax-highlighting"
      "nvm"
      "mole"
      # SketchyBar (custom menu bar)
      "sketchybar"
    ];

    # GUI apps
    casks = [
      # ===== Terminals =====
      "iterm2"
      "warp"
      "ghostty" # GPU-accelerated terminal (Mitchell Hashimoto)

      # ===== Productivity - Core =====
      "raycast" # Launcher (replaces Spotlight)
      "aerospace" # i3-like tiling window manager
      "orbstack" # Docker Desktop replacement (10x faster)
      "obsidian" # Knowledge management / PKM
      "granola" # AI meeting notes (no bot joins)

      # ===== Productivity - Recommended =====
      "notion-calendar" # Calendar management (formerly Cron)
      "loom" # Async video messages
      "arc" # Modern browser for devs
      "cleanshot" # Screenshots & screen recording
      "jordanbaird-ice" # Menu bar management (free)

      # ===== macOS UI Enhancements =====
      "karabiner-elements" # Keyboard remapping & Hyper Key
      "linearmouse" # Fix mouse acceleration, per-device settings
      "dockdoor" # Window previews on Dock hover
      # "alt-tab" # Windows-style Alt+Tab with previews (disabled: broken upstream cask definition)
      "hazeover" # Dim inactive windows for focus
      "stats" # System stats in menu bar
      "bettertouchtool" # Trackpad gestures & window snapping
      "ubersicht" # Desktop widgets (HTML/CSS)

      # ===== Quick Look Plugins =====
      "syntax-highlight" # Code syntax in Quick Look
      "qlmarkdown" # Markdown preview in Finder
      "qlstephen" # Plain text files without extension
      "qlvideo" # Video thumbnails & previews

      # ===== Screencasting & Demo =====
      "keycastr" # Show keystrokes on screen

      # ===== Fonts =====
      "font-jetbrains-mono-nerd-font"
      "font-fira-code-nerd-font"
      "font-hack-nerd-font"
      "font-meslo-lg-nerd-font"

      # ===== Editors =====
      "zed"
      "visual-studio-code"

      # ===== Dev Tools =====
      "mactex-no-gui"
      "postman"
      "proxyman"

      # ===== Communication =====
      "discord"
      "slack"

      # ===== Gaming =====
      "steam"

      # ===== Home Server =====
      "tailscale" # VPN mesh network
      "lm-studio" # Local LLM inference
    ];

    # Mac App Store apps (requires: mas CLI installed and signed into App Store)
    # Find app IDs: mas search <app> or https://apps.apple.com
    masApps = {
      # "Xcode" = 497799835;  # Uncomment if needed (large download)
      # "1Password for Safari" = 1569813296;
    };
  };

  # macOS system preferences
  system = {
    stateVersion = 5;

    defaults = {
      # Dock
      dock = {
        autohide = true;
        autohide-delay = 0.0;
        autohide-time-modifier = 0.4;
        orientation = "bottom";
        show-recents = false;
        tilesize = 48;
      };

      # Finder
      finder = {
        AppleShowAllExtensions = true;
        AppleShowAllFiles = true;
        FXDefaultSearchScope = "SCcf"; # Current folder
        FXEnableExtensionChangeWarning = false;
        FXPreferredViewStyle = "Nlsv"; # List view
        ShowPathbar = true;
        ShowStatusBar = true;
      };

      # Global
      NSGlobalDomain = {
        AppleKeyboardUIMode = 3; # Full keyboard access
        ApplePressAndHoldEnabled = false; # Key repeat instead of accents
        InitialKeyRepeat = 15;
        KeyRepeat = 2;
        NSAutomaticCapitalizationEnabled = false;
        NSAutomaticDashSubstitutionEnabled = false;
        NSAutomaticPeriodSubstitutionEnabled = false;
        NSAutomaticQuoteSubstitutionEnabled = false;
        NSAutomaticSpellingCorrectionEnabled = false;
      };

      # Trackpad
      trackpad = {
        Clicking = true; # Tap to click
        TrackpadRightClick = true;
      };
    };

    # Keyboard settings
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = true;
    };
  };

  # Networking
  networking = {
    hostName = hostname;
    computerName = hostname;
  };

  # Users (declarative)
  users.users.${username} = {
    name = username;
    home = "/Users/${username}";
  };
}
