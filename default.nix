# This default.nix builds a tarball containing a library archive and some manpages.  
# Likely to only work on linux.
#
# Just run 'nix-build' and fish the tarball out of 'result/'.
#
# For the Haskell dependencies that diverge from our pinned Nixpkgs,
# we use cabal2nix like thus:
#
#  $ cabal2nix cabal://sexp-grammar-2.2.1 > nix/sexp-grammar.nix
#
# And then import them into the configuration.  Although note that
# Nixpkgs also tends to contain the newest version of each Hackage
# package, even if it is not the default.
#
# To update the Nixpkgs snapshot (which also includes tooling), use:
#
#  $ niv update nixpkgs -b master
#
# Also remember this guide: https://github.com/Gabriel439/haskell-nix/blob/master/project1/README.md
