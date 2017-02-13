{ reflex-platform ? import ../deps/reflex-platform {}
, ghcjs
, shared ? import ../shared
}:

let
  # new_ghcjs2 = pkgs.haskell.packages.ghcjs.override {
  #   packageSetConfig = self: super: {
  #     fast-logger = reflex-platform.nixpkgs.haskellPackages
  #       src = pkgs.fetchFromGitHub {
  #         owner = "TaktInc";
  #         repo = "logger";
  #         rev = "dc0e5d622ea6c891d6d42bf3d8e051d94c202896";
  #         sha256 = "13ifaf7fcpizpgh8qgb1wm3rap62kzz8blhvnc3ay6vrg32flydm";
  #       } + "/fast-logger";
  #   };
  # };

  new_ghcjs = reflex-platform.ghcjs.override {
    overrides = self: super: {
      fast-logger = reflex-platform.nixpkgs.haskell.lib.overrideCabal super.fast-logger (old: {
        postPatch = old.postPatch or "" + ''
          # remove the Safe extensions, since ghcjs-boot directory
          # doesn’t provide Trustworthy
          sed -ie '/LANGUAGE Safe/d' System/Log/FastLogger/*.hs
          cat System/Log/FastLogger/Date.hs
        '';
      });
    };
  };
  drv = new_ghcjs.callPackage ./packages.nix {
    shared = shared { compiler = new_ghcjs; };
  };
in
  if reflex-platform.nixpkgs.pkgs.lib.inNixShell then drv.env else drv
