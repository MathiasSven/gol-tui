{
  description = "Conway's Game of Life TUI";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {self, nixpkgs, systems, ...}: let
    forEachSystem = func:
      builtins.foldl' 
      nixpkgs.lib.recursiveUpdate { }
      (map func (import systems));
  in forEachSystem (system: 
  
  let
    pkgs = nixpkgs.legacyPackages.${system}.extend overlay;
    pkgsStatic = pkgs.pkgsStatic;
    
    overlay = self: super: {
      haskell = super.haskell // {
        packageOverrides = self: super: {
          gol-tui = 
            let pkg = self.callCabal2nix "gol-tui" src { };
            in pkgs.haskell.lib.overrideCabal pkg (old: { 
              executableSystemDepends = with pkgs; [ numcpp boost.dev ];
            });
        };
      };
    };
    
    inherit (pkgs) lib;

    regexes =
          [ ".*.cabal$" "^src.*" "LICENSE" ];
    src = builtins.path {
      path = ./.;
      name = "gol-tui-src";
      filter = path: type:
        let relPath = lib.removePrefix (toString ./. + "/") (toString path);
        in lib.any (re: builtins.match re relPath != null) regexes;
    };

  in {
    packages.${system} = rec { 
      default = gol-tui;
      gol-tui = pkgs.haskellPackages.gol-tui;
      gol-tui-static = pkgsStatic.haskellPackages.gol-tui;
      
      gol-tui-shell = default.env.overrideAttrs (oldAttrs: {
        name = "gol-tui";

        buildInputs = oldAttrs.buildInputs ++ (with pkgs; [
          numcpp
          boost.dev
          cabal-install
          haskell-language-server
          hlint
          nil
        ]);
      });
    };

    devShells.${system}.default = self.packages.${system}.gol-tui-shell;
  });
}

