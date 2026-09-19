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
          star-git = pkgs.sbcl.buildASDFSystem {
            pname = "star-git";
            version = "0.1.0";
            src = self;
            systems = [ "star-git" ];
            nativeLibs = [ pkgs.xz.out ];
            lispLibs = [
              tek9
              cl.ironclad
              cl.babel
              cl.cffi
            ];
          };
          sbclWithTek9 = pkgs.sbcl.withPackages (_: [ tek9 ]);
          tek9IngestWorker = pkgs.writeShellApplication {
            name = "tek9-ingest-worker";
            runtimeInputs = [ sbclWithTek9 ];
            text = ''
              if [[ -z "''${TEK9_DB_PATH:-}" ]]; then
                echo "TEK9_DB_PATH is required" >&2
                exit 64
              fi
              exec sbcl --noinform --disable-debugger --non-interactive \
                --eval '(require :asdf)' \
                --eval '(asdf:load-system :tek9)' \
                --eval '(tek9:run-ingest-worker (uiop:parse-native-namestring (uiop:getenv "TEK9_DB_PATH")))'
            '';
          };
        in
        {
          default = tek9;
          inherit tek9 star-git;
          tek9-ingest-worker = tek9IngestWorker;
        });

      apps = eachSystem (system: {
        ingest-worker = {
          type = "app";
          program = "${self.packages.${system}.tek9-ingest-worker}/bin/tek9-ingest-worker";
        };
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
              xz
            ];
            shellHook = ''
              export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.lmdb.out pkgs.openssl pkgs.xz.out ]}:''${LD_LIBRARY_PATH:-}
            '';
          };
        });

      checks = eachSystem (system:
        let
          pkgs = import nixpkgs { inherit system; };
          tek9 = self.packages.${system}.tek9;
          starGit = self.packages.${system}.star-git;
          sbclWithTek9 = pkgs.sbcl.withPackages (_: [ tek9 ]);
          tek9IngestWorker = self.packages.${system}.tek9-ingest-worker;
          sbclWithStarGit = pkgs.sbcl.withPackages (_: [ starGit ]);
        in
        {
          package = tek9;
          star-git-package = starGit;

          ingest-worker-smoke = pkgs.runCommand "tek9-ingest-worker-smoke" {
            nativeBuildInputs = [ tek9IngestWorker pkgs.jq ];
          } ''
            export HOME="$TMPDIR/home"
            export TEK9_DB_PATH="$TMPDIR/tek9-ingest/"
            mkdir -p "$HOME"

            printf '%s\n' '{"op":"status","source_id":"navidrome"}' \
              | tek9-ingest-worker \
              | jq -e '.ok == true and .generation == 0 and .watermark == null' >/dev/null

            test -f "$TEK9_DB_PATH/data.mdb"
            touch "$out"
          '';

          package-smoke = pkgs.runCommand "tek9-package-smoke" {
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

          star-git-package-smoke = pkgs.runCommand "star-git-package-smoke" {
            nativeBuildInputs = [ sbclWithStarGit ];
          } ''
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME" "$TMPDIR/consumer"
            cd "$TMPDIR/consumer"

            cat > "$TMPDIR/star-git-smoke.lisp" <<'LISP'
            (require :asdf)
            (asdf:load-system :star-git)
            (assert (find-package :star-git))

            (let* ((root (uiop:ensure-directory-pathname
                          (pathname (uiop:getenv "TMPDIR"))))
                   (source-path (merge-pathnames #P"star-git-source/" root))
                   (restored-path (merge-pathnames #P"star-git-restored/" root))
                   (pack-path (merge-pathnames #P"archive/smoke.sgp.lzma" root))
                   (bytes (babel:string-to-octets
                           "{\"dtype\":\"note\",\"smoke\":true}"
                           :encoding :utf-8))
                   (source (star-git:open-repository source-path)))
              (unwind-protect
                   (let* ((blob-id (star-git:write-blob source bytes))
                          (commit-id
                            (star-git:commit-document
                             source
                             "smoke-doc"
                             blob-id
                             :tenant "smoke"
                             :dataset "nix"
                             :dtype "note"
                             :mutation-kind "create"
                             :accepted-at 4000000000)))
                     (multiple-value-bind (pack-id metadata)
                         (star-git:build-pack source
                                              (list blob-id commit-id)
                                              pack-path)
                       (declare (ignore metadata))
                       (assert (string= pack-id
                                        (star-git:pack-id-for-path pack-path)))
                       (let ((restored
                               (star-git:open-repository restored-path)))
                         (unwind-protect
                              (progn
                                (multiple-value-bind (imported-pack-id ignored)
                                    (star-git:import-pack restored pack-path)
                                  (declare (ignore ignored))
                                  (assert (string= pack-id imported-pack-id)))
                                (assert (equalp bytes
                                                (star-git:read-blob restored
                                                                    blob-id)))
                                (let ((history
                                        (star-git:log-document restored
                                                               "smoke-doc")))
                                  (assert (= 1 (length history)))
                                  (assert (string=
                                           commit-id
                                           (getf (first history) :commit-id))))
                                (assert (null (star-git:fsck restored))))
                           (star-git:close-repository restored)))))
                (star-git:close-repository source)))
            LISP

            env -u LD_LIBRARY_PATH sbcl --noinform --non-interactive \
              --load "$TMPDIR/star-git-smoke.lisp"

            test -f "$TMPDIR/star-git-source/data.mdb"
            test -f "$TMPDIR/star-git-restored/data.mdb"
            test -s "$TMPDIR/archive/smoke.sgp.lzma"
            touch "$out"
          '';
        });
    };
}
