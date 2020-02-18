# repology-api

Scans the Repology [nixpkgs-unstable] distribution for packages that have newer
versions on at least one other Repology distribution, and emits a JSON file with
the upgrade information.

Note that because this is not diffing against the master checkout of Nixpkgs,
the JSON file will include packages that have already been updated on master but
not yet reached the unstable Nixpkgs channel.

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

[nixpkgs-unstable]: https://repology.org/repository/nix_unstable
