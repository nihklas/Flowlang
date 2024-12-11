{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    zig.url = "github:mitchellh/zig-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    ...
  } @ inputs:
    flake-utils.lib.eachDefaultSystem
    (
      system: let
        overlays = [inputs.zig.overlays.default];
        pkgs = import nixpkgs {
          inherit system overlays;
        };
      in
        with pkgs; {
          devShells.default = mkShell {
            nativeBuildInputs = [
              zigpkgs.master
            ];
            shellHook = ''
              # We unset some NIX environment variables that might interfere with the zig compiler.
              # Issue: https://github.com/ziglang/zig/issues/18998
              unset NIX_CFLAGS_COMPILE
              unset NIX_LDFLAGS
              printf '\n'
              echo "Running Zig Version: $(zig version)"
            '';
          };

          checks.default = stdenv.mkDerivation {
            name = "all-tests";
            nativeBuildInputs = [zigpkgs.master];
            src = ./.;
            preBuild = "export HOME=$TMPDIR";
            installPhase = ''
              runHook preInstall
              zig build --prefix $out check
              zig build --prefix $out unit-test
              zig build --prefix $out integration-test
              mkdir $out
              runHook postInstall
            '';
          };
        }
    );
}
