{ reflex-platform ? import ../reflex-platform {}
, ghcjs
, shared ? import ../shared
}:
let 
  drv = ghcjs.callPackage ./packages.nix {
  shared = shared { compiler = ghcjs; };
};
in
  if reflex-platform.nixpkgs.pkgs.lib.inNixShell then drv.env else drv
