#! /usr/bin/env nix-shell
#! nix-shell ci.nix -i bash

bower install

pulp build --include test

node ./test/index.mjs
