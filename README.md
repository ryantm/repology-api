# repology-api

This will eventually just be an API binding for Repology, but I'm using this to help me make a tool for nixpkgs, so they are combined right now. It returns all the packages that have newer versions in a packaging repo that Repology tracks.

Example usage:
```
cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api
boehm-gc 7.6.2 7.6.4
buildbot-worker 0.9.11 0.9.15
arc-theme 2017-05-12 20180114
blueman 2.0.4 2.1
bossa 2014-08-18 20140109
...
```
