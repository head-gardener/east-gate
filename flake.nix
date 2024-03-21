{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" ];
      imports = [ ];
      flake = { };

      perSystem = { self', system, pkgs, package, ... }: {
        _module.args.package = hpack: hpack.developPackage {
          root = ./.;
          name = "east-gate";
        };

        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          overlays = [
            (_: prev: {
              haskellPackages = prev.haskellPackages.extend (_: super: {
                east-gate = package super;
              });
            })
          ];
        };

        checks = {
          build = self'.packages.default;
        };

        devShells.default = with pkgs.haskellPackages; shellFor {
          nativeBuildInputs = [
            cabal-install
            hpack
            haskell-language-server
          ];

          packages = p: [ p.east-gate ];

          withHoogle = true;
        };

        packages.default = pkgs.haskellPackages.east-gate;
        packages.east-gate = self'.packages.default;
      };
    };
}
