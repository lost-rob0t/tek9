{
  description = "Fast embedded Common Lisp document and graph database on LMDB.";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      eachSystem = f: nixpkgs.lib.genAttrs systems (system: f system);
    in
    {
      packages = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
          cl = pkgs.sbcl.pkgs;
          tek9 = pkgs.sbcl.buildASDFSystem {
            pname = "tek9";
            version = "0.2.0";
            src = self;
            systems = [ "tek9" ];
            nativeLibs = [ pkgs.lmdb ];
            lispLibs = [
              cl.alexandria
              cl."bordeaux-threads"
              cl.serapeum
              cl.jsown
              cl.lmdb
              cl."cl-conspack"
            ];
          };
        in
        {
          default = tek9;
          inherit tek9;
        });

      devShells = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              pkg-config
              sbcl
              glib
              openssl
              lmdb
            ];
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.lmdb pkgs.openssl ]}:''${LD_LIBRARY_PATH:-}
            '';
          };
        });

      checks = eachSystem (system: {
        package = self.packages.${system}.tek9;
      });
    };
}
