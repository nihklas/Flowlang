{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    zls.url = "github:nihklas/zls/0.14.0";
    zls.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    nixpkgs,
    zls,
    self,
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
        packages =
          [
            zls.packages.${system}.default
            pkgs.zig
          ]
          ++ (
            if system == "x86_64-linux"
            then [pkgs.valgrind]
            else []
          );
      };

      pipeline = pkgs.mkShellNoCC {
        packages = [pkgs.zig];
      };
    });

    packages = eachSystem ({
      pkgs,
      target,
      system,
    }: let
      defaultConfig = {
        name,
        mode,
      }: {
        name = name;
        src = ./.;
        nativeBuildInputs = [pkgs.zig];
        buildPhase = ''
          zig build -Doptimize=${mode} -Dtarget=${target} --global-cache-dir "$(mktemp -d)"
        '';
        installPhase = ''
          mkdir -p $out/bin
          mv zig-out/bin/compiler $out/bin/flowc
        '';
      };
    in {
      safe = pkgs.stdenvNoCC.mkDerivation (defaultConfig {
        name = "flow-safe";
        mode = "ReleaseSafe";
      });
      fast = pkgs.stdenvNoCC.mkDerivation (defaultConfig {
        name = "flow-fast";
        mode = "ReleaseFast";
      });
      small = pkgs.stdenvNoCC.mkDerivation (defaultConfig {
        name = "flow-small";
        mode = "ReleaseSmall";
      });

      default = self.packages.${system}.fast;
    });
  };
}
