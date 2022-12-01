{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [ pkgs.clojure pkgs.babashka pkgs.leiningen ];
}
