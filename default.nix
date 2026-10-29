let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
in
{ pkgs ? import sources.nixpkgs {}
  # Use the default GHC from nixpkgs, which has the best binary cache coverage.
  # To use a specific GHC instead, pass e.g. pkgs.haskell.packages.ghc912
, haskellPackages ? pkgs.haskellPackages
}:
haskellPackages.callCabal2nix "gnc"
  # Only include files known by git (i.e. exclude dist-newstyle/, result, etc)
  (pkgs.lib.fileset.toSource {
    root = ./.;
    fileset = pkgs.lib.fileset.gitTracked ./.;
  })
  {}
