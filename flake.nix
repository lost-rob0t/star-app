{
  description = "StarIntel UI consuming the generated StarLang contract";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/e6eae2ee2110f3d31110d5c222cd395303343b08";
    star-cl = {
      url = "github:lost-rob0t/star-cl/a18a6cddcbcf4d989495a5de8abcedc4d0294d1c";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    starintel-server = {
      url = "github:lost-rob0t/starintel-server/117057d6f5bbf2e2b40d5cfd89eb7adcca5e28fd";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.star-cl.follows = "star-cl";
    };
  };
  outputs = { self, nixpkgs, starintel-server, star-cl }:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
      # ASDF loads source paths at runtime, so patch the immutable source input
      # as well as the compiled library. Check macro characters through the
      # public accessor, rather than treating SBCL's non-macro markers as functions.
      namedReadtablesSource = pkgs.runCommand "named-readtables-sbcl-source" {} ''
        cp -r ${pkgs.sbclPackages.named-readtables.src} "$out"
        chmod -R u+w "$out"
        substituteInPlace "$out/src/cruft.lisp" --replace-fail \
          '(if reader-fn' '(if (get-macro-character char readtable)'
      '';
      sbcl' = pkgs.sbcl.withOverrides (lispSelf: lispSuper: {
        named-readtables = lispSuper.named-readtables.overrideAttrs (old: {
          src = namedReadtablesSource;
        });
      });
      runtimeLibs = with pkgs; [ openssl sqlite lmdb rabbitmq-c libffi ];
      starintel-client = starintel-server.packages.${system}.starintel-gserver-client;
      star-app = sbcl'.buildASDFSystem {
        pname = "star-app"; version = "0.1.0"; src = ./source;
        nativeLibs = runtimeLibs;
        lispLibs = (with sbcl'.pkgs; [ dexador clog clack clack-handler-hunchentoot lack
          hunchentoot jsown jzon log4cl str serapeum local-time ])
          ++ [ starintel-client star-cl.packages.${system}.starintel ];
        systems = [ "star-app" ]; asdFilesToKeep = [ "star-app.asd" ]; dontStrip = true;
      };
      sbcl-wrapped = sbcl'.withPackages (ps: [ star-app ]);
      binary = pkgs.stdenv.mkDerivation {
        pname = "star-app"; version = "0.1.0";
        dontUnpack = true; dontStrip = true;
        nativeBuildInputs = [ pkgs.makeWrapper ];
        buildPhase = ''
          ${sbcl-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
            --eval '(require :asdf)' --eval '(asdf:load-system :star-app)' \
            --eval '(sb-ext:save-lisp-and-die "star-app" :toplevel (function star.app:main) :executable t :compression t)'
        '';
        installPhase = ''
          mkdir -p "$out/bin"
          install -m755 star-app "$out/bin/star-app"
          wrapProgram "$out/bin/star-app" --prefix LD_LIBRARY_PATH : ${pkgs.lib.makeLibraryPath runtimeLibs}
        '';
      };
    in {
      packages.${system} = { default = binary; star-app-lib = star-app; sbcl-wrapped = sbcl-wrapped; };
      checks.${system} = {
        browser = pkgs.runCommand "star-app-installed-browser" {
          nativeBuildInputs = [ (pkgs.python3.withPackages (p: [ p.playwright ])) pkgs.chromium ];
        } ''
          export XDG_CACHE_HOME="$TMPDIR/browser-cache"
          export XDG_CONFIG_HOME="$TMPDIR/browser-config"
          mkdir -p "$XDG_CACHE_HOME" "$XDG_CONFIG_HOME"
          python3 ${self}/t/browser.py ${binary}/bin/star-app ${pkgs.chromium}/bin/chromium
          touch "$out"
        '';
        application = binary;
        contract = pkgs.runCommand "star-app-canonical-contract" {
          nativeBuildInputs = [ pkgs.python3 sbcl-wrapped ];
        } ''
          cd ${self}
          python3 scripts/sync-starintel-schema.py --offline
          python3 ${self}/scripts/check-runtime-release.py ${star-cl} ${starintel-server}
          python3 ${self}/t/http_wire.py ${sbcl-wrapped}/bin/sbcl
          touch "$out"
        '';
      };
      devShells.${system}.default = pkgs.mkShell {
        packages = [ sbcl-wrapped pkgs.python3 ];
        LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath runtimeLibs;
      };
    };
}
