{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Blockchain.Simulation
import Blockchain.Structures
import qualified Data.Map as Map
import System.Directory
import Data.ConfigFile as ConfigFile
import Data.Either.Utils


outChain :: BlockChain -> String
outChain chain = concat [chainInfo, diffInfo]
    where
        diffInfo = concat ["Cumulative difficulty: ", show $ cumulativeDifficulty chain]
        chainInfo = foldl (\s b -> concat [s, show $ blockTimestamp b, " -> ", show $ generator b, "--",
                                            show $ blockId b ," -- ", show $ baseTarget b, " -- ", show $ length $ transactions b, "\n "]) "" chain

outChainNode :: Node -> String
outChainNode node = outChain $ bestChain node


outTxs :: Node -> String
outTxs node = foldl (\s t -> concat [s, show t, "\n"]) "" txs
    where
        chain = bestChain node
        txs = concat $ map transactions $ chain

outConnection :: Network -> String
outConnection network = concat ps
    where
        cons = connections network
        ks = Map.keys cons
        ps = map (\k -> concat [show $ nodeId $ k, " -> ", show $ Map.findWithDefault [] k cons, "\n "]) ks

commonChainLength :: Node -> Node -> Int
commonChainLength n1 n2 = length $ commonChain (bestChain n1) (bestChain n2)

commonChainsNode :: Node -> [Node] ->  String
commonChainsNode n ns = show (let others = filter (\_ -> True) ns in map (commonChainLength n) others)

commonChains :: [Node] -> String
commonChains ns = show $ map (\n -> concat[show $ nodeId n," : ",show $ selfBalance n ,"<->", commonChainsNode n ns]) ns

nodeBalances :: Node -> [Node] -> String
nodeBalances node ns = let bals =  map (accBalance node) $ map account ns in
  concat [show (nodeId node), "->" , show $ (sum bals): bals]

allBalances :: [Node] -> String
allBalances nodes = concat $ map (\n -> nodeBalances n nodes ++ "\n") nodes   

main = do
    putStrLn "Starting cryptocurrency simulation..."

    val <- ConfigFile.readfile ConfigFile.emptyCP "params.conf"
    let cp = forceEither val

    let outdir = forceEither $ ConfigFile.get cp "DEFAULT" "outdir"

    let initSimData = SimulationData{
        timestamp = 0,
        simulationId = forceEither $ ConfigFile.get cp "DEFAULT" "simulation-id",
        maxConnectionsPerNode = forceEither $ ConfigFile.get cp "DEFAULT" "max-connections-per-node",
        addNodeAvgGap = forceEither $ ConfigFile.get cp "DEFAULT" "add-node-avg-gap",
        deadline = forceEither $ ConfigFile.get cp "DEFAULT" "duration"}

    createDirectoryIfMissing True outdir

    let network = snd $ goThrouhTimeline (initSimData, genesisState)
    let ns = nodes network

    writeFile (concat [outdir, "/network"]) (show network)
    writeFile (concat [outdir, "/cons"]) (outConnection network)
    mapM_ (\i -> writeFile (concat [outdir, "/node", show i]) (show (ns !! i))) [0..(length ns - 1)]
    mapM_ (\i -> writeFile (concat [outdir, "/chain", show i]) (outChainNode $ ns !! i)) [0..(length ns - 1)]
    mapM_ (\i -> writeFile (concat [outdir, "/txs", show i]) (outTxs $ ns !! i)) [0..(length ns - 1)]
    mapM_ (\i -> writeFile (concat [outdir, "/common", show i]) (outChain $ commonChain (bestChain $ ns !! i) (bestChain $ ns !! (i+1)))) [0..(length ns - 2)]

    putStrLn "Final balances(from self point of view):"
    putStrLn $ show $ map selfBalance ns

    putStrLn "\n"
    putStrLn "Node Id : Self balance <-> Common chain lengths with other nodes: "
    let cc = commonChains ns
    writeFile (concat [outdir, "/commons"]) cc
    putStrLn $ cc

    let bb = allBalances ns
    writeFile (concat [outdir, "/balances"]) bb
    putStrLn $ bb

    putStrLn "\n Cryptocurrency simulation has been finished"