{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    haskell-flake.url = "github:srid/haskell-flake";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        inputs.haskell-flake.flakeModule
      ];

      perSystem = { self', pkgs, lib, config, ... }:
        let
          projectRoot = builtins.toString (lib.fileset.toSource {
            root = ./.;
            fileset = lib.fileset.unions [
              ./exe
              ./lib
              ./specs
              ./docs
              ./daffm.cabal
            ];
          });
          installFiles = [
            { source = ./docs/daffm.1; target = "$out/share/man/man1/daffm.1"; }
          ];
          configurationFlags = [
            "--ghc-options=-O2"
          ];
          buildInputs = with pkgs; [
            pkg-config
          ];

          devPackages = with pkgs; [ just pandoc ];
        in {
          haskellProjects.default = {
            inherit projectRoot;

            packages = {};
            settings = {
              daffm = {
                check = true;
                deadCodeElimination = true;
                staticLibraries = true;
                strip = true;
                custom = drv:
                  (pkgs.haskell.lib.compose.appendConfigureFlags configurationFlags drv).overrideAttrs (old: {
                    postInstall = ''
                      ${toString (map (f: ''
                        install -Dm644 "${f.source}" "${f.target}";
                      '') installFiles)}
                    '';
                  })
                ;
              };
            };

            devShell = {
              hlsCheck.enable = true;
            };

            autoWire = [ "packages" "apps" "checks" ];
          };

          packages.default = self'.packages.daffm;
          apps.default = self'.apps.daffm;

          devShells.default = pkgs.mkShell {
            inputsFrom = [
              config.haskellProjects.default.outputs.devShell
            ];
            packages = devPackages;
            inherit buildInputs;

            LD_LIBRARY_PATH = "${pkgs.lib.makeLibraryPath buildInputs}";
          };
        };
    };
}
