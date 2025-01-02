{
  inputs = {
    toolchain = {
      url = "github:myclevorname/nix-calculators";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, toolchain, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system: {
        packages.default = toolchain.legacyPackages.${system}.buildCEProgram {
          name = "famicalc";
          src = self;
        };
      });
}
