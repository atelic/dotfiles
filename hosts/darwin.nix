# System-level macOS configuration
{ pkgs, inputs, username, hostname, ... }:

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
      cleanup = "zap";  # Remove unlisted packages
      upgrade = true;
    };
    
    taps = [
      # homebrew/services is deprecated and now built-in
    ];

    # CLI tools - migrate your brew formulas here over time
    brews = [
      # Essential tools you currently have via brew
      "zsh-syntax-highlighting"
      "nvm"
      # Add more as you migrate...
    ];

    # GUI apps
    casks = [
      "iterm2"
      "warp"
      "discord"
      "postman"
      # Add more as you migrate...
    ];
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
        FXDefaultSearchScope = "SCcf";  # Current folder
        FXEnableExtensionChangeWarning = false;
        FXPreferredViewStyle = "Nlsv";  # List view
        ShowPathbar = true;
        ShowStatusBar = true;
      };
      
      # Global
      NSGlobalDomain = {
        AppleKeyboardUIMode = 3;  # Full keyboard access
        ApplePressAndHoldEnabled = false;  # Key repeat instead of accents
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
        Clicking = true;  # Tap to click
        TrackpadRightClick = true;
      };
    };
    
    # Keyboard settings
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToControl = false;  # Set to true if you want this
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
