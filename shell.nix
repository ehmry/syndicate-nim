{ pkgs ? import <nixpkgs> { } }:
pkgs.nim2Packages.buildNimPackage { name = "dummy"; }
