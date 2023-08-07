#!/bin/sh
rm -rf dist-newstyle

cabal build\
  --with-ghc /home/hsyl20/projects/ghc/master/_build/stage1/bin/javascript-unknown-ghcjs-ghc\
  --with-ghc-pkg /home/hsyl20/projects/ghc/master/_build/stage1/bin/javascript-unknown-ghcjs-ghc-pkg\
  --allow-newer
