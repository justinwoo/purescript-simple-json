#! /usr/bin/env nix-shell
#! nix-shell ci.nix -i bash

npx --yes spago@1.0.5 build &&
node ./test/index.mjs
