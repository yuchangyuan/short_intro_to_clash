{ mkDerivation, aeson, aeson-pretty, ansi-terminal, array, async
, attoparsec, base, base16-bytestring, binary, bytestring
, clash-prelude, concurrent-supply, containers, cryptohash-sha256
, data-binary-ieee754, data-default, deepseq, directory, dlist
, exceptions, extra, filepath, ghc, ghc-boot-th
, ghc-typelits-knownnat, Glob, hashable, haskell-src-exts
, haskell-src-meta, hint, integer-gmp, interpolate, lens, lib, mtl
, ordered-containers, pretty-show, prettyprinter, primitive
, quickcheck-text, stringsearch, tasty, tasty-hunit
, tasty-quickcheck, template-haskell, temporary, terminal-size
, text, time, transformers, trifecta, unordered-containers, vector
, vector-binary-instances, yaml
}:
mkDerivation {
  pname = "clash-lib";
  version = "1.6.1";
  sha256 = "f2428566d99b1d714728330e6b1613356147e37a9304db549d9a864ee8dbf061";
  isLibrary = true;
  isExecutable = true;
  enableSeparateDataOutput = true;
  libraryHaskellDepends = [
    aeson aeson-pretty ansi-terminal array async attoparsec base
    base16-bytestring binary bytestring clash-prelude concurrent-supply
    containers cryptohash-sha256 data-binary-ieee754 data-default
    deepseq directory dlist exceptions extra filepath ghc ghc-boot-th
    hashable haskell-src-meta hint integer-gmp interpolate lens mtl
    ordered-containers pretty-show prettyprinter primitive
    template-haskell temporary terminal-size text time transformers
    trifecta unordered-containers vector vector-binary-instances yaml
  ];
  executableHaskellDepends = [
    aeson base bytestring containers deepseq directory Glob
    stringsearch yaml
  ];
  testHaskellDepends = [
    aeson aeson-pretty base base16-bytestring bytestring clash-prelude
    concurrent-supply containers data-default deepseq ghc
    ghc-typelits-knownnat haskell-src-exts lens pretty-show
    quickcheck-text tasty tasty-hunit tasty-quickcheck template-haskell
    text transformers unordered-containers
  ];
  homepage = "https://clash-lang.org/";
  description = "Clash: a functional hardware description language - As a library";
  license = lib.licenses.bsd2;
}
