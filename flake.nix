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
          lmdb = cl.lmdb.overrideLispAttrs (old: {
            nativeLibs = (old.nativeLibs or [ ]) ++ [ pkgs.lmdb.out ];
          });
          tek9 = pkgs.sbcl.buildASDFSystem {
            pname = "tek9";
            version = "0.2.0";
            src = self;
            systems = [ "tek9" ];
            nativeLibs = [ pkgs.lmdb.out ];
            lispLibs = [
              cl.alexandria
              cl."bordeaux-threads"
              cl.serapeum
              cl.jsown
              lmdb
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
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.lmdb.out pkgs.openssl ]}:''${LD_LIBRARY_PATH:-}
            '';
          };
        });

      checks = eachSystem (system: {
        package = self.packages.${system}.tek9;
        package-smoke =
          let
            pkgs = import nixpkgs { inherit system; };
            tek9 = self.packages.${system}.tek9;
            sbclWithTek9 = pkgs.sbcl.withPackages (_: [ tek9 ]);
          in
          pkgs.runCommand "tek9-package-smoke" {
            nativeBuildInputs = [ sbclWithTek9 ];
          } ''
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME" "$TMPDIR/consumer"
            cd "$TMPDIR/consumer"

            sbcl --noinform --non-interactive \
              --eval '(require :asdf)' \
              --eval '(asdf:load-system :tek9)' \
              --eval '(assert (find-package :tek9))' \
              --eval "(let ((database (tek9:open-database (tek9:new-database \"smoke\" :path #P\"$TMPDIR/database/\")))) (tek9:close-database database))"

            test -f "$TMPDIR/database/data.mdb"
            touch "$out"
          '';
      });
    };
}
