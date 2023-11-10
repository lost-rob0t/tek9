{
  description = "A NoSql Database in common lisp built ontop of LMDB.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
  let
    pkgs = nixpkgs.legacyPackages.x86_64-linux;
  in {
    devShell.x86_64-linux =
      pkgs.mkShell {
        buildInputs = with pkgs; [
          pkg-config
          sbcl
          #lmdb.out
          glib
          openssl
        ];
        shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.lmdb.out])}:${pkgs.lib.makeLibraryPath([pkgs.openssl])}
            '';
      };
  };
}
