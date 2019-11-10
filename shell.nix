{
  pkgs ? import ./nix/nixpkgs/default.nix {},
}:


with pkgs;

env {
  buildInputs = [ stack2nix ];
}
