{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, easy-purescript-nix, ... }@inputs:
    let
      name = "purescript-graphql-fundeps";

      supportedSystems = [
        "aarch64-darwin"
        "x86_64-darwin"
        "x86_64-linux"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
    in
    {
      devShell = forAllSystems (system:
        let
          pkgs = import nixpkgs { inherit system; };

          easy-ps = import easy-purescript-nix { inherit pkgs; };
        in
        pkgs.mkShell {
          inherit name;
          buildInputs = (with pkgs; [
            nodejs_20
            nixpkgs-fmt
          ]) ++ (with easy-ps; [
            purs
            purs-tidy
            psa
            spago
            purescript-language-server
          ]) ++ (pkgs.lib.optionals (system == "aarch64-darwin")
            (with pkgs.darwin.apple_sdk.frameworks; [
              Cocoa
              CoreServices
            ]));
        });
    };
}
