{ system ? builtins.currentSystem
, pins ? import ./npins
, pkgs ? import pins.nixpkgs { inherit system; }
}:

let
  watch-tests = pkgs.writeScriptBin "watch-tests" ''
    ${pkgs.ghcid}/bin/ghcid --clear \
    --command "cabal repl sillylang:test:tests" \
    --test "hspec spec" \
    --setup "import Test.Hspec" \
    --restart=./src --restart=./test
  '';
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    niv
    nixpkgs-fmt
    ghc
    cabal-install
    ormolu
    hlint
    haskell-language-server
    pcre
    ghcid
    haskellPackages.cabal-fmt
    haskellPackages.haskell-ci
    haskellPackages.hspec
    watch-tests
  ];
}
