{
  description = "Eric's nix-darwin + home-manager configuration";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    nix-darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    # Homebrew integration - lets nix-darwin manage brew packages declaratively
    nix-homebrew.url = "github:zhaofengli/nix-homebrew";

    # Fixes Spotlight indexing for Nix-installed apps
    mac-app-util.url = "github:hraban/mac-app-util";

    # Clawdbot - AI assistant gateway
    nix-clawdbot.url = "github:clawdbot/nix-clawdbot";
  };

  outputs =
    inputs@{
      self,
      nixpkgs,
      nix-darwin,
      home-manager,
      nix-homebrew,
      mac-app-util,
      nix-clawdbot,
    }:
    let
      username = "eric";
      hostname = "Erics-Mac-mini";
      system = "aarch64-darwin"; # Apple Silicon
    in
    {
      darwinConfigurations.${hostname} = nix-darwin.lib.darwinSystem {
        specialArgs = { inherit inputs username hostname; };
        modules = [
          # Homebrew integration
          nix-homebrew.darwinModules.nix-homebrew
          {
            nix-homebrew = {
              enable = true;
              user = username;
              enableRosetta = true; # Support x86_64 brew formulas
              autoMigrate = true; # Migrate existing brew installation
            };
          }

          # Spotlight indexing for Nix apps
          mac-app-util.darwinModules.default

          # System configuration
          ./hosts/darwin.nix

          # Allow unfree packages (e.g., claude-code)
          { nixpkgs.config.allowUnfree = true; }

          # Home Manager
          home-manager.darwinModules.home-manager
          {
            home-manager = {
              useGlobalPkgs = true;
              useUserPackages = true;
              backupFileExtension = "backup"; # Backup existing files instead of failing
              extraSpecialArgs = { inherit inputs username; };
              users.${username} = import ./home/default.nix;
              sharedModules = [
                mac-app-util.homeManagerModules.default
                nix-clawdbot.homeManagerModules.clawdbot
              ];
            };
          }
        ];
      };
    };
}
