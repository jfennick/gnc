let
  sources = import ./nix/sources.nix {};
in
{ pkgs ? import sources.nixpkgs {}
, haskellPackages ? pkgs.haskellPackages
}:
haskellPackages.shellFor {
  packages = _: [ (import ./default.nix { inherit pkgs haskellPackages; }) ];
  nativeBuildInputs = [
    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.haskell-language-server
  ];
}
