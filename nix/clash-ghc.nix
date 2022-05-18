{ mkDerivation, array, base, bytestring, Cabal, clash-lib
, clash-prelude, concurrent-supply, containers, data-binary-ieee754
, deepseq, directory, extra, filepath, ghc, ghc-boot, ghc-prim
, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, ghci, hashable, haskeline, integer-gmp
, lens, lib, mtl, primitive, process, reflection, split
, template-haskell, text, time, transformers, uniplate, unix
, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "clash-ghc";
  version = "1.6.1";
  sha256 = "02c6431626ba31b2187e9a6cc03f4bb117301c1daa0fef5f393a00c8c4cd7c4e";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring Cabal clash-lib clash-prelude
    concurrent-supply containers data-binary-ieee754 deepseq directory
    extra filepath ghc ghc-boot ghc-prim ghc-typelits-extra
    ghc-typelits-knownnat ghc-typelits-natnormalise ghci hashable
    haskeline integer-gmp lens mtl primitive process reflection split
    template-haskell text time transformers uniplate unix
    unordered-containers utf8-string vector
  ];
  executableHaskellDepends = [ base ];
  homepage = "https://clash-lang.org/";
  description = "Clash: a functional hardware description language - GHC frontend";
  license = lib.licenses.bsd2;
}
