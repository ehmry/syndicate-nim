{ pkgs ? import <nixpkgs> { } }:

pkgs.buildNimPackage {
  name = "dummy";
  lockFile = ./lock.json;
  buildInputs = builtins.attrValues { inherit (pkgs) getdns solo5; };
  nativeBuildInputs = builtins.attrValues { inherit (pkgs) pkg-config solo5; };
}
