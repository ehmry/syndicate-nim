let
  syndicate = builtins.getFlake "syndicate";
  pkgs =
    import <nixpkgs> { overlays = builtins.attrValues syndicate.overlays; };
in pkgs.nimPackages.syndicate
