pkgs: self: super:
with pkgs.haskell.lib;
{
  # skip check, see https://github.com/haskell/cabal/issues/7890
  clash-ghc        = dontCheck (self.callPackage ./clash-ghc.nix {});
  clash-lib        = dontCheck (self.callPackage ./clash-lib.nix {});
  clash-prelude    = dontCheck (self.callPackage ./clash-prelude.nix {});
  doctest-parallel = dontCheck (self.callPackage ./doctest-parallel.nix {});
}
