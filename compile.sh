#/bin/sh
ghc -rtsopts -threaded -O2 -static -optl-pthread -optl-static -XStandaloneDeriving -O2 blockchain.hs simulation.hs launcher.hs