{
  description = "streamly-archive";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        ghcVersion = "9103";
        packageName = "streamly-archive";
        config = {};

        overlays = [
          (final: prev:
            let
              haskellPkgs = final.haskell.packages."ghc${ghcVersion}";
            in {
              myHaskellPkgs = haskellPkgs.override {
                overrides = hfinal: hprev: {
                  ${packageName} = hfinal.callCabal2nix "${packageName}" ./. {
                    archive = final.pkgs.libarchive;
                  };

                  streamly = hfinal.callHackageDirect {
                    pkg = "streamly";
                    ver = "0.11.1";
                    sha256 = "sha256-4h1MwaN7eXMvzXKyjggIjjR3BlsGzl4vfCO7VBGGvrc=";
                  } {};
                  streamly-core = hfinal.callHackageDirect {
                    pkg = "streamly-core";
                    ver = "0.3.1";
                    sha256 = "sha256-k9h+I74GNsluf55hJFDZiLwEO2x9moFvtCarCeCpaa4=";
                  } {};
                };
              };

              ${packageName} = final.myHaskellPkgs.${packageName};

              myDevShell = final.myHaskellPkgs.shellFor {
                packages = p: [p.${packageName}];
                nativeBuildInputs = [
                  final.myHaskellPkgs.cabal-install
                  final.myHaskellPkgs.haskell-language-server
                  final.myHaskellPkgs.ormolu
                ];

                # Without this, "cabal repl" shows "libarchive.so: cannot open shared...". See also:
                # https://discourse.nixos.org/t/shared-libraries-error-with-cabal-repl-in-nix-shell/8921
                shellHook = ''
                  export LD_LIBRARY_PATH=${final.pkgs.libarchive.lib}/lib
                '';
              };
            })
        ];

        pkgs = import nixpkgs { inherit config overlays system; };
      in {
        packages = {
          default = pkgs.${packageName};
          ${packageName} = pkgs.${packageName};
          "${packageName}-ci" =
            pkgs.haskell.lib.overrideCabal
              pkgs.${packageName}
              (drv: { testFlags = ["--quickcheck-tests=500"]; });
        };
        
        devShells.default = pkgs.myDevShell;
      });
}
