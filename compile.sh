#/bin/sh
ghc -rtsopts -threaded -O2 -static -optl-pthread -optl-static -XStandaloneDeriving -O2 constants.hs blockchain.hs simulation.hs launcher.hs