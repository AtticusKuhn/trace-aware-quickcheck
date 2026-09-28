{
  description = "A tiny categorical Fibonacci interpreter with execution traces";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = nixpkgs.lib.genAttrs systems;

      projectFor = system:
        let
          pkgs = import nixpkgs { inherit system; };
          ghc = pkgs.haskell.packages.ghc96.ghcWithPackages (packages: [ packages.random ]);
          demo = pkgs.stdenv.mkDerivation {
            pname = "trace-fibonacci";
            version = "0.1.0";
            src = pkgs.lib.cleanSource ./.;
            nativeBuildInputs = [ ghc pkgs.makeWrapper ];
            buildPhase = ''
              runHook preBuild
              ghc -O1 -Wall -Werror -outputdir build -o trace-fibonacci Main.hs
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              install -Dm755 trace-fibonacci "$out/bin/trace-fibonacci"
              wrapProgram "$out/bin/trace-fibonacci" \
                --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.graphviz ]}
              runHook postInstall
            '';
            meta.mainProgram = "trace-fibonacci";
          };
        in {
          inherit pkgs ghc demo;
        };
    in {
      packages = forAllSystems (system: {
        default = (projectFor system).demo;
      });

      apps = forAllSystems (system: {
        default = {
          type = "app";
          program = "${(projectFor system).demo}/bin/trace-fibonacci";
        };
      });

      devShells = forAllSystems (system:
        let project = projectFor system;
        in {
          default = project.pkgs.mkShell {
            packages = [ project.ghc project.pkgs.graphviz project.pkgs.gnuplot ];
          };
        });

      checks = forAllSystems (system:
        let project = projectFor system;
        in {
          fibonacci = project.pkgs.runCommand "trace-fibonacci-check" { } ''
            LC_ALL=C.UTF-8 ${project.demo}/bin/trace-fibonacci > "$out"
          '';
          collapsed-graphs = project.pkgs.stdenv.mkDerivation {
            pname = "trace-fibonacci-collapsed-graphs-check";
            version = "0.1.0";
            src = project.pkgs.lib.cleanSource ./.;
            nativeBuildInputs = [ project.ghc ];
            buildPhase = ''
              runHook preBuild
              ghc -O1 -Wall -Werror -outputdir build -main-is Tests \
                -o collapsed-graphs-check Tests.hs Main.hs
              ./collapsed-graphs-check
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              touch "$out"
              runHook postInstall
            '';
          };
          algorithm1-fib = project.pkgs.stdenv.mkDerivation {
            pname = "trace-fibonacci-algorithm1-check";
            version = "0.1.0";
            src = project.pkgs.lib.cleanSource ./.;
            nativeBuildInputs = [ project.ghc ];
            buildPhase = ''
              runHook preBuild
              ghc -O1 -Wall -Werror -outputdir build -main-is Algorithm1Fib \
                -o algorithm1-fib Algorithm1Fib.hs Main.hs
              ./algorithm1-fib 200 1
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              touch "$out"
              runHook postInstall
            '';
          };
        });
    };
}
