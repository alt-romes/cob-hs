#!/bin/sh
cabal build cob-swift $(echo $STATIC_HASKELL_CABAL_OPTS)
