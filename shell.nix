{ pkgs ? (import <nixpkgs> {}) }:
with pkgs;
mkShell {
  packages = [
    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format
    elmPackages.elm-doc-preview
    elmPackages.elm-language-server
    nodejs_22
  ];
}
