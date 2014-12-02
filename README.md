Forging Simulation
=================

Simulation tool for Nxt-like 100% Proof-of-Stake forging


Compile & Run
-------------

To compile you need:

1. GHC haskell compiler.
   `sudo apt-get install ghc`

2. cabal-install for external haskell packages installation
   `sudo apt-get install cabal-install`

3. ConfigFile package, which can be locally installed
   `cabal install ConfigFile`

4. Then run `compile.sh` shell-script for Linux to compile the sources

5. The executable name is `launcher` unless another name is manually specified


CONFIGURATION
-------------

Configuration file 'params.conf' (the name is statically defined in launcher.hs, after changing re-compilation
 is needed) consists of the following parameters:

* _outdir_  - folder results will be put into. Will be created if not exists


* _duration_ - test duration, in seconds. Please note, performance is not very good at the moment, so better
    start with default value on an unknown machine


* _simulation-id_ - being using as seed value for random number generator. Results for same simulation-id & duration values are
 the same from run to run, so change this value to start new experiment. other researcher can follow your results by
 using the same `simulation-id` value


* _max-connections-per-node_ - max number of outgoing connections a node could has. please note we use
one-directional connections, so sometimes oldest connection being dropped to avoid network clusterization


* _add-node-avg-gap_ - average gap between node creation events, e.g. if add-node-avg-gap == 1000 && duration = 100000
 then ~100 nodes will be created during the test


Results
-------

After finishing simulation program dumps out some results to files in output directory set by `outdir` parameter
(`out` by default):

* _nodeX_ - final state of node X

* _chainX_ - chain of node X at final moment

* _commonX_ - common chain for nodes X and X+1

* _txsX_ - confirmed transactions presenting in a blockchain of node X

* _cons_ - network connections in final moment of a test in format nodeId -> [otherNodeId1...otherNodeIdN]

* _network_ - very big file with whole network's final state

* _commons_ - lengths of common chains for all nodes


Also some information being printing after test to the console:

* final self-balances for all nodes(i.e. every node prints balance from it's own point of view)

*  lengths of common chains for all nodes(the same goes to `commons` file)



CONTACTS & Discussion
---------------------

Please contact kushti, kushtech@yahoo.com for any reasonable questions or suggestions about the program.
Also please visit our subforum @ Nxt forum : https://nxtforum.org/consensus-research/


