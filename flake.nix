{
  description = "StarIntel UI - Web interface for StarIntel";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    starintel-server = {
      url = "path:/home/unseen/Documents/Projects/starintelV4/starintel-server";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, starintel-server }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      runtimeLibs = with pkgs; [
        openssl sqlite lmdb rabbitmq-c libffi
      ];

      # Get the starintel-gserver-client library from the upstream flake
      starintel-client = starintel-server.packages.${system}.starintel-gserver-client;
      starintel-gserver = starintel-server.packages.${system}.starintel-gserver;

      # Build star-app using standard sbcl
      star-app = pkgs.sbcl.buildASDFSystem rec {
        pname = "star-app";
        version = "0.1.0";
        src = ./source;

        nativeLibs = runtimeLibs;

        lispLibs = with pkgs.sbclPackages; [
          dexador
          clog
          clack
          clack-handler-hunchentoot
          lack
          hunchentoot
          jsown
          log4cl
          str
        ] ++ [ starintel-client ];

        systems = [ "star-app" ];

        asdFilesToKeep = [ "star-app.asd" ];

        dontStrip = true;
      };

      # Create wrapper with all dependencies
      sbcl-wrapped = pkgs.sbcl.withPackages (ps: [ star-app ]);

    in {
      packages.${system} = {
        default = pkgs.stdenv.mkDerivation {
          pname = "star-app";
          version = "0.1.0";

          dontUnpack = true;
          dontStrip = true;
          nativeBuildInputs = [ pkgs.makeWrapper ];

          buildPhase = ''
            ${sbcl-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(asdf:load-system :star-app)" \
              --eval "(sb-ext:save-lisp-and-die \"star-app\" :toplevel 'star.app:main :executable t :compression t)"
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp star-app $out/bin/
            wrapProgram $out/bin/star-app \
              --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath runtimeLibs}"
          '';
        };

        star-app-lib = star-app;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          sbcl-wrapped pkg-config
        ] ++ runtimeLibs;

        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
          unset LD_PRELOAD
          echo "Star-App dev environment ready"
          echo "Use: sbcl to start SBCL with all dependencies"
          echo ""
          echo "To load the system in SBCL:"
          echo "  (asdf:load-system :star-app)"
          echo ""
          echo "To run the application:"
          echo "  (star.app:main)"
        '';
      };
    };
}
