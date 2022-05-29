{ mkDerivation, array, arrows, base, binary, bytestring
, constraints, containers, criterion, data-binary-ieee754
, data-default-class, deepseq, directory, doctest-parallel, extra
, filepath, ghc-prim, ghc-typelits-extra, ghc-typelits-knownnat
, ghc-typelits-natnormalise, half, hashable, hedgehog, hint
, integer-gmp, interpolate, lens, lib, QuickCheck
, quickcheck-classes-base, recursion-schemes, reflection
, singletons, tasty, tasty-hedgehog, tasty-hunit, tasty-quickcheck
, tasty-th, template-haskell, text, th-abstraction, th-lift
, th-orphans, time, transformers, type-errors, uniplate, vector
}:
mkDerivation {
  pname = "clash-prelude";
  version = "1.6.3";
  sha256 = "f5e7ab6309f9b1db73693abd2fcd38c20f1c4e8a5a7b45fd3f12cbb54ca780fa";
  libraryHaskellDepends = [
    array arrows base binary bytestring constraints containers
    data-binary-ieee754 data-default-class deepseq extra ghc-prim
    ghc-typelits-extra ghc-typelits-knownnat ghc-typelits-natnormalise
    half hashable integer-gmp interpolate lens QuickCheck
    recursion-schemes reflection singletons template-haskell text
    th-abstraction th-lift th-orphans time transformers type-errors
    uniplate vector
  ];
  testHaskellDepends = [
    base bytestring deepseq doctest-parallel filepath
    ghc-typelits-extra ghc-typelits-knownnat ghc-typelits-natnormalise
    hedgehog hint quickcheck-classes-base tasty tasty-hedgehog
    tasty-hunit tasty-quickcheck tasty-th template-haskell
  ];
  benchmarkHaskellDepends = [
    base criterion deepseq directory template-haskell
  ];
  homepage = "https://clash-lang.org/";
  description = "Clash: a functional hardware description language - Prelude library";
  license = lib.licenses.bsd2;
}
