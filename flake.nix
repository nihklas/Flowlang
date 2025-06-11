{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    zls.url = "github:nihklas/zls/0.14.0";
    zls.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    zls,
    ...
  }: let
    systems = ["aarch64-darwin" "x86_64-linux"];
    eachSystem = function:
      nixpkgs.lib.genAttrs systems (system:
        function {
          inherit system;
          target = builtins.replaceStrings ["darwin"] ["macos"] system;
          pkgs = nixpkgs.legacyPackages.${system};
        });
  in {
    devShells = eachSystem ({
      system,
      pkgs,
      ...
    }: {
      default = pkgs.mkShellNoCC {
        packages = [
          zls.packages.${system}.default
          pkgs.zig
        ];
      };

      pipeline = pkgs.mkShellNoCC {
        packages = [pkgs.zig];
      };
    });

    packages = eachSystem ({
      pkgs,
      target,
      ...
    }: {
      default = pkgs.stdenvNoCC.mkDerivation {
        name = "flowc";
        src = ./.;
        nativeBuildInputs = [pkgs.zig];
        buildPhase = ''
          zig build -Doptimize=ReleaseSafe -Dtarget=${target} --global-cache-dir "$(mktemp -d)"
        '';
        installPhase = ''
          mkdir -p $out/bin
          mv zig-out/bin/compiler $out/bin/flowc
        '';
      };
    });
  };
}
