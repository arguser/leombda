{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, parsec, stdenv }:
      mkDerivation {
        pname = "leombda";
        version = "0.1.1";
        src = ./.;
        libraryHaskellDepends = [ base parsec ];
        homepage = "https://github.com/k0001/leombda";
        description = "Leo's lambda calculus interpreter";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
