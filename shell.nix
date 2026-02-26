{ pkgs ? import sources.nixpkgs { }
, sources ? import ./npins
}:

with pkgs;

mkShell {
  packages = [
    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format
    elmPackages.elm-doc-preview
    elmPackages.elm-language-server
    nodejs_20
  ];
}
