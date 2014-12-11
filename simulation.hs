{-# LANGUAGE ScopedTypeVariables #-}

module Blockchain.Simulation where
import Blockchain.Structures
import qualified Data.ByteString.Lazy as B
import qualified Data.Word as W
import qualified Data.Random.List()
import Data.Maybe
import System.Random
import qualified Data.Map as Map


data SimulationData = SimulationData {
    timestamp :: Timestamp,
    deadline :: Timestamp,
    maxConnectionsPerNode :: Int,
    addNodeAvgGap :: Int,
    simulationId :: Int
}

incTimestamp :: SimulationData -> SimulationData
incTimestamp sd = sd{timestamp = t + 1} where t = timestamp sd

notExpired :: SimulationData -> Bool
notExpired sd = (timestamp sd) < (deadline sd)

simpleGen :: SimulationData -> StdGen
simpleGen sd = mkStdGen $ (simulationId sd) * (timestamp sd + 1)

nodeGen :: SimulationData -> Node ->  StdGen
nodeGen sd nd = mkStdGen $ (simulationId sd) * (timestamp sd + 1) * (nodeId nd)




createNode :: Account -> Node
createNode acc = Node{localView = genesisView, unconfirmedTxs=[], account=acc}

rescan :: [Block] -> Node -> Node
rescan chain node = pushBlocks (tail chain) node{localView = genesisView, unconfirmedTxs = []}


-----------Bootstrapping functions----------------------


earlyInvestors :: [Account]
earlyInvestors = map (\i -> generateAccount $ mkStdGen i) [0..20]


godAccount :: Account
godAccount = Account {publicKey = B.pack[18, 89, -20, 33, -45, 26, 48, -119, -115, 124, -47, 96, -97, -128, -39, 102,
                                        -117, 71, 120, -29, -39, 126, -108, 16, 68, -77, -97, 12, 68, -46, -27, 27]}



genesisBlock :: Block
genesisBlock = block where
    amt = systemBalance `div` (length earlyInvestors)
    genesisTxs = map (\ acc -> Transaction{sender=godAccount, recipient=acc, amount = amt, fee=0, txTimestamp=0}) earlyInvestors
    block = Block{transactions = genesisTxs, blockTimestamp = 0, baseTarget = maxBaseTarget,
                         generator = godAccount, generationSignature = B.replicate 64 0}


genesisView :: LocalView
genesisView = LocalView{balances = genBalances, nodeChain = [genesisBlock]}
    where
        genBalances = processBlock genesisBlock initialBalances
        initialBalances = Map.singleton godAccount systemBalance


genesisState :: Network
genesisState = Network {nodes = [createNode $ head earlyInvestors], connections = Map.empty}




-----------Helper functions----------------------

randomBytes :: Int -> StdGen -> [W.Word8]
randomBytes 0 _ = []
randomBytes count g = fromIntegral value:randomBytes (count - 1) nextG
                      where (value, nextG) = next g

randomByteString :: Int -> StdGen -> B.ByteString
randomByteString count g = B.pack $ randomBytes count g


generatePK :: StdGen -> B.ByteString
generatePK gen = randomByteString 32 gen


accountByPK :: B.ByteString -> Account
accountByPK pk = Account {publicKey=pk}

generateAccount :: StdGen -> Account
generateAccount gen = accountByPK $ generatePK gen


-----------Simulating functions ----------------------


--for now 1 node == 1 account
addAccount :: Network -> Network
addAccount _ = error "not impl"



generateConnections :: SimulationData -> Network -> Network
generateConnections sd network = network{connections = updCons} where
    ns = nodes network
    nsCount = length ns
    cons = connections network
    updCons = foldl (\cs n -> let out = outgoingConnections network n in
            if (length out) < (min (maxConnectionsPerNode sd)  nsCount-1)
                then
                    let gen = nodeGen sd n in
                    let rndNode = ns !! (fst $ randomR (0, nsCount - 1) gen) in
                        if (nodeId rndNode /= nodeId n) && (not $ elem rndNode out)
                            then Map.insert n (out++[rndNode]) cs
                            else cs
                else cs
        ) cons ns


dropConnections :: SimulationData -> Network -> Network
dropConnections sd network = case (timestamp sd) `mod` 60 of
            0 -> do
                let cons = connections network in
                    let updCons = foldl (\cs n -> let out = outgoingConnections network n in
                            if (length out) > 10
                                then Map.insert n (tail out) cs
                                else cs ) cons (nodes network) in
                    network{connections = updCons}
            _ -> network


randomNeighbour :: SimulationData -> Node -> Network -> Maybe Node
randomNeighbour sd node network = if nbcnt == 0 then Nothing
                                    else let idx = fst $ randomR (0, nbcnt - 1) (nodeGen sd node) in
                                        Just $ neighbours !! idx
                                  where
                                    neighbours = outgoingConnections network node
                                    nbcnt = length neighbours




addNode :: SimulationData -> Network -> Network
addNode sd initNetwork = case rnd  of
                            1 -> initNetwork {nodes = nodes initNetwork ++ [createNode $ generateAccount gen]}
                            _ -> initNetwork
                        where
                            gen = simpleGen sd
                            rnd::Int = fst $ randomR (0, addNodeAvgGap sd) gen




downloadBlocksFrom :: Node -> Node -> Network -> Network
downloadBlocksFrom node otherNode network = updNetwork
    where
        chain = nodeChain $ localView node
        otherChain = nodeChain $ localView otherNode
        otherChainLength = length otherChain
        updNetwork = if cumulativeDifficulty otherChain > cumulativeDifficulty chain -- todo: check NRS
                                             then do
                                                 let common = commonChain chain otherChain []
                                                 let commonChainLength = length common
                                                 let blocksNumToDl = min (otherChainLength - commonChainLength) maxBlocksFromPeer
                                                 let newBlocks = drop commonChainLength $ take (commonChainLength+blocksNumToDl) otherChain
                                                 let modifiedNode = rescan (common ++ newBlocks) node
                                                 updateNode modifiedNode network
                                             else network


downloadBlocks :: SimulationData -> Node -> Network -> Network
downloadBlocks sd node network = resNetwork
    where
        mbUpdNeighbour = randomNeighbour sd node network
        resNetwork = case mbUpdNeighbour of
                Just neighbour -> downloadBlocksFrom node neighbour network
                Nothing -> network


downloadBlocksNetwork :: SimulationData -> Network ->  Network
downloadBlocksNetwork sd network = foldl (\nw n -> downloadBlocks sd n nw) network (nodes network)


sendTransactionsOut :: SimulationData -> Node -> Network ->  Network
sendTransactionsOut td node network = case randomNeighbour td node network of
                    Just neighbour -> let txsToSend = unconfirmedTxs node in
                                      let otherTxs = unconfirmedTxs neighbour in
                                      let updNeighbourTxs = foldl (\txs tx -> if(elem tx txs) then txs else txs ++ [tx]) otherTxs txsToSend in
                                      updateNode neighbour{unconfirmedTxs = updNeighbourTxs} network
                    Nothing -> network



propagateTransactions :: SimulationData -> Network ->  Network
propagateTransactions sd network = foldl (\nws sndr -> sendTransactionsOut sd sndr nws) network senders
            where senders = filter (\n -> (length $ unconfirmedTxs n) > 0) (nodes network)




sendBlockOut :: SimulationData -> Node -> Network -> Network
sendBlockOut td node network  = resNetwork
            where
                block = lastNodeBlock node
                cons = outgoingConnections network node
                clen = length cons
                resNetwork = if clen == 0 then network
                    else do
                        let otherNode = cons !! (fst $ randomR (0, clen - 1) $ simpleGen td)
                        let chain = nodeChain $ localView node
                        let otherChain = nodeChain $ localView otherNode
                        let prevId = blockId $ chain !! ((length chain)-2)
                        let otherLastId = blockId $ last otherChain
                        if (length chain >= 2) &&
                           (otherLastId == prevId) &&
                           (length chain == length otherChain +1) -- && (cumulativeDifficulty chain > cumulativeDifficulty otherChain)
                                then updateNode (processIncomingBlock block otherNode) network
                                else network

propagateLastBlock :: SimulationData -> Network -> Network
propagateLastBlock sd network = foldl (\nw sndr -> sendBlockOut sd sndr nw) network senders
    where
        senders = filter (\n ->
                let lastBl = lastNodeBlock n in
                    (timestamp sd - blockTimestamp lastBl) < 15 && generator lastBl /= account n
            ) (nodes network)





generateTransactionsForNode :: SimulationData -> Node -> Network -> Node
generateTransactionsForNode td node network = node{unconfirmedTxs = unconfirmedTxs node ++ [tx]}
    where
        gen = nodeGen td node
        ns = nodes network
        amt = fst $ randomR (1 , (selfBalance node) `div` 2) gen
        rcp = account $ ns !! (fst $ randomR (0, length ns - 1) gen)
        tstamp = timestamp td
        tx = Transaction{sender = account node, recipient = rcp, amount = amt, fee=1, txTimestamp = tstamp}



generateTransactionsForNodes :: SimulationData -> [Node] -> Network -> [Node]
generateTransactionsForNodes sd nonEmpty network = foldl (\ns n ->
        let gen = nodeGen sd n in
        let r::Int = fst $ randomR (0, 10) gen in case r of
                1 -> let updNode::Node = generateTransactionsForNode sd n network in ns ++ [updNode]
                _ -> ns ++ [n]
    ) [] nonEmpty



generateTransactions :: SimulationData -> Network -> Network
generateTransactions td network = network{nodes = emptyNodes ++ updNonEmpty} where
    (nonEmptyNodes, emptyNodes) = foldl (\(nens,ens) n ->
            if selfBalance n > minFee*200 then (nens ++ [n], ens) else (nens, ens++[n])) ([],[]
        ) (nodes network)
    updNonEmpty = generateTransactionsForNodes td nonEmptyNodes network




forgersList :: Timestamp -> Network -> (Network, [Node])
forgersList tstamp sys  = (sys{nodes = forgers ++ nonForgers}, forgers) where
    (nonForgers, forgers) = foldl (\(ons, newns) n -> let mbForged = forge n tstamp in case isJust mbForged of
        True -> (ons, newns ++ [fromJust mbForged])
        False -> (ons ++ [n], newns)) ([],[]) (nodes sys)



networkForge :: SimulationData -> Network -> Network
networkForge sd nw = let (nwf, forgers) = forgersList (timestamp sd) nw in
    foldl (\nws frg -> sendBlockOut sd frg nws) nwf forgers



--dirty hack :(
addInvestorNode :: SimulationData -> Network -> Network
addInvestorNode sd network = case timestamp sd of
            1000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 1]}
            2000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 2]}
            4000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 3]}
            5000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 4]}
            6000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 5]}
            7000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 6]}
            7500 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 7]}
            8000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 8]}
            9000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 9]}
            10000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 10]}
            12000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 11]}
            14000 -> network{nodes = nodes network ++ [createNode $ earlyInvestors !! 12]}
            _ -> network





systemTransform :: SimulationData -> Network -> Network
systemTransform sd network = networkForge sd $ propagateTransactions sd $ generateTransactions sd $
            propagateLastBlock sd $ downloadBlocksNetwork sd $
            dropConnections sd $ generateConnections sd $ addNode sd $ addInvestorNode sd network




goThrouhTimeline :: (SimulationData, Network) -> (SimulationData, Network)
goThrouhTimeline (sd, nw) | notExpired sd =
        goThrouhTimeline (sdinc, (systemTransform sdinc nw)) where sdinc = incTimestamp sd
goThrouhTimeline (sd, nw) = (sd, nw)