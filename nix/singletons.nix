{ mkDerivation, base, lib }:
mkDerivation {
  pname = "singletons";
  version = "3.0.1";
  sha256 = "d3aece000c43469da339f72c6396dfd969f250d50a217f67d4e20b003b4f0f53";
  revision = "1";
  editedCabalFile = "0n3jr9jqz50ygaw80a9cx3g09w7w8bdlq9ssyap0a4990yxl8fbx";
  libraryHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "Basic singleton types and definitions";
  license = lib.licenses.bsd3;
}
