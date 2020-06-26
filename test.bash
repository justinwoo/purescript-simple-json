#! /usr/bin/env nix-shell
#! nix-shell ci.nix -i bash

bower install

# NOTE: The following bug has been fixed, but the fix hasn't been upstreamed
# to the version of nixpkgs pinned in this repo:
#
# https://github.com/purescript-contrib/pulp/issues/392
#
# Once pulp has been updated, drop '--no-check-main'
pulp test --no-check-main
