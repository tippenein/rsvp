let
  pkgs = <nixpkgs>;
  pkg-frontend =
    { mkDerivation, reflex, reflex-dom, shared, fast-logger
    }:
    mkDerivation {
      pname = "frontend";
      version = "0.1.0.0";
      src = ./.;
      isLibrary = false;
      isExecutable = true;
      executableHaskellDepends = [
        reflex reflex-dom shared fast-logger
      ];
    };
  new_ghcjsHEAD = pkgs.haskell.packages.ghcjsHEAD.override {
    packageSetConfig = self: super: {
      fast-logger = pkgs.haskell.lib.overrideCabal super.fast-logger (old: {
        src = pkgs.fetchFromGitHub {
          owner = "TaktInc";
          repo = "logger";
          rev = "dc0e5d622ea6c891d6d42bf3d8e051d94c202896";
          sha256 = "13ifaf7fcpizpgh8qgb1wm3rap62kzz8blhvnc3ay6vrg32flydm";
        } + "/fast-logger";
      });
      frontend = self.callPackage pkg-frontend {};
    };
  };
in
  new_ghcjsHEAD.frontend

# let
#   config = {
#     packageOverrides = pkgs: rec {
#       haskellPackages = pkgs.haskellPackages.override {
#         overrides = haskellPackagesNew: haskellPackagesOld: rec {
#           project2 =
#             haskellPackagesNew.callPackage ./default.nix {
#               tar = pkgs.libtar;
#             };
#         };
#       };
#     };
#   };
#
#   pkgs = import <nixpkgs> { inherit config; };
#
# in
#   {
#     project2 = pkgs.haskellPackages.project2;
#   }
