# repology-api

Example usage:
```
cabal2nix --shell --hpack . > shell.nix && nix-build shell.nix && result/bin/repology-api
Update needed: boehm-gc 7.6.2 (nix_unstable)-> 7.6.4 (homebrew)
Update needed: buildbot-worker 0.9.11 (nix_unstable)-> 0.9.15 (aur)
Update needed: arc-theme 2017-05-12 (nix_unstable)-> 20180114 (pureos_landing)
Update needed: blueman 2.0.4 (nix_unstable)-> 2.1 (fedora_rawhide)
Update needed: bossa 2014-08-18 (nix_unstable)-> 20140109 (pkgsrc_current)
```
