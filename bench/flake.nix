{
  description = "streamly-archive-bench";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "927";
        packageName = "streamly-archive-bench";
        config = {};

        overlays = [
          (final: prev:
            let
              haskellPkgs = final.haskell.packages."ghc${ghcVersion}";
            in {
              myHaskellPkgs = haskellPkgs.override {
                overrides = hfinal: hprev: {
                  ${packageName} =
                    hfinal.callCabal2nix packageName ./. {
                      # Use local streamly-archive in "../".
                      streamly-archive =
                        hfinal.callCabal2nix "streamly-archive" ../. {
                          streamly = hfinal.streamly_0_9_0;
                          archive = pkgs.libarchive;
                        };
                      
                      archive = pkgs.libarchive;
                    };
                };
              };

              ${packageName} = final.myHaskellPkgs.${packageName};

              myDevShell = final.myHaskellPkgs.shellFor {
                packages = p: [p.${packageName}];
                nativeBuildInputs = [
                  final.myHaskellPkgs.cabal-install
                  final.myHaskellPkgs.haskell-language-server
                  final.pkgs.pcre
                ];
              };
            })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        packages = {
          default = pkgs.${packageName};
          ${packageName} = pkgs.${packageName};
        };
        
        devShells.default = pkgs.myDevShell;
      });
}
