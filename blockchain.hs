module Blockchain.Structures where
import Constants 
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as BI
import qualified Data.Map as Map

-- Account-based proof-of-stake cryptocurrency model

type Timestamp = Int

first8bytesAsNumber :: B.ByteString -> Integer
first8bytesAsNumber bs =  fromIntegral $ BI.runGet BI.getWord64le first8 where first8 = B.take 8 bs

-- tfdepth > 1 multibranch 
--         = 1 singlebranch                                                                              
--         = 0 not forging                                                                               
--         < 0 full multibranch (please do not use - exponential growth)                                                                       
data Account =
    Account {
        publicKey :: B.ByteString,
        tfdepth :: Int
    }

instance Show Account where show acc = show $ accountId acc
instance Eq Account where a1 == a2  = accountId a1 == accountId a2
instance Ord Account where compare a1 a2 = compare (accountId a1) (accountId a2)

accountId :: Account -> Int
accountId acc = fromIntegral $ first8bytesAsNumber $ publicKey acc

data Transaction =
    Transaction {
        sender :: Account,
        recipient :: Account,
        amount :: Int,
        fee :: Int,
        txTimestamp :: Timestamp
    }  deriving (Show)

instance Eq Transaction where t1 == t2  = sender t1 == sender t2
                                          && recipient t1 == recipient t2
                                          && txTimestamp t1 == txTimestamp t2
                                          
instance Ord Transaction where compare t1 t2 = compare (txTimestamp t1) (txTimestamp t2) 

validate :: Transaction -> Bool
validate _ = True

data Block =
    Block {
      -- need to move to LocalView
      --  allTransactions :: [Transaction],
        transactions :: [Transaction],
        baseTarget :: Integer,
      -- theoretically need to move to LocalView  
        totalDifficulty :: Double,
        generator :: Account,
        generationSignature :: B.ByteString,
        blockTimestamp :: Timestamp
    }  deriving (Show)

--removed (blockTimestamp)
blockId :: Block -> Int
blockId block = fromIntegral $ first8bytesAsNumber $ generationSignature block

-- todo: add more checks?
isGenesis :: Block -> Bool
isGenesis b = blockTimestamp b == 0

--instance Show Block where show b = show $ blockId b
instance Eq Block where b1 == b2  = blockId b1 == blockId b2
instance Ord Block where compare b1 b2 = compare (blockId b1) (blockId b2)

initialBaseTarget :: Integer
initialBaseTarget = div maxHit (2*goalBlockTime*(fromIntegral systemBalance))

maxBaseTarget :: Integer
maxBaseTarget = initialBaseTarget * (fromIntegral systemBalance)

-- rewritten as we now maintain the whole history, better do it by Map?
containsTx :: Block -> LocalView -> Transaction -> Bool
containsTx block view tx = elem tx $ Map.findWithDefault [] block $ blockTransactions view

calcGenerationSignature :: Block -> Account -> B.ByteString
calcGenerationSignature prevBlock acct = SHA.bytestringDigest $ SHA.sha256 $ B.append (generationSignature prevBlock) pk
    where pk = publicKey acct

--correct this!
difficultyFunction :: Integer -> Double
difficultyFunction x = two64/(fromIntegral x)

formBlock :: Block -> Account -> Timestamp -> [Transaction] -> Block
formBlock prevBlock gen timestamp txs =
    Block {transactions = newTxs, blockTimestamp = timestamp, 
           baseTarget = bt, totalDifficulty = td, generator = gen, 
           generationSignature = gs}
    where newTxs = filter validate txs
          -- move to the caller
          -- newTxs = filter (\tx -> not $ containsTx prevBlock tx) newTxs'
          prevTarget = baseTarget prevBlock
          maxTarget = min (2*prevTarget)  maxBaseTarget
          minTarget = max (prevTarget `div` 2)  1
          candidate = prevTarget*(toInteger (timestamp - (blockTimestamp prevBlock))) `div` goalBlockTime
          bt = min (max minTarget candidate) maxTarget
          gs = calcGenerationSignature prevBlock gen -- is gs 64 bytes?
          -- move to the caller?
          td = (totalDifficulty prevBlock) + (difficultyFunction bt)

type BlockChain = [Block]

-- from the next to parent
type BlockTree = Map.Map Block Block

-- check if pb isn't included
updateView :: Block -> Block -> LocalView -> LocalView
updateView pb b view = if (Map.notMember b $ blockTree view) then 
                        if (Map.member pb $ blockTree view) || (isGenesis pb) then 
                           let prBal = Map.findWithDefault Map.empty pb $ blockBalances view in
                           let prTxs = Map.findWithDefault []        pb $ blockTransactions view in 
                           view {blockTree      = Map.insert b pb $ blockTree view, 
                              blockBalances     = Map.insert b (processBlock b prBal) $ blockBalances view,
                              blockTransactions = Map.insert b (prTxs ++ (transactions b)) $ blockTransactions view}
                        -- need to add more logic when prevBlock not found - try to download it or whatever   
                        else view   
                       else                        
                        view 
                         

cumulativeDifficulty :: BlockChain -> Double
cumulativeDifficulty chain = foldl (\cd b -> cd + (difficultyFunction $ baseTarget b)) 0 chain

cumulativeNodeDifficulty :: Node -> Double
cumulativeNodeDifficulty node = totalDifficulty $ bestBlock node


-- type BlockTree = [BlockChain]

data LocalView =
    LocalView {
        -- nodeChain :: BlockChain,
        blockTree :: BlockTree,
        blockBalances :: Map.Map Block (Map.Map Account Int),
        -- all the transactions from the genesis, better do it as Block->Transaction mapping?
        blockTransactions :: Map.Map Block [Transaction]
    } deriving (Show)


accountBalance :: LocalView -> Block -> Account -> Int
accountBalance view b acc = Map.findWithDefault 0 acc (Map.findWithDefault Map.empty b (blockBalances view))

effectiveBalance :: LocalView -> Block -> Account -> Int
effectiveBalance view b acc = accountBalance view b acc -- todo: simplification, no 1440 blocks waiting for now

addMoney ::  Int -> Account ->  Map.Map Account Int -> Map.Map Account Int
addMoney diff acc blns = let oldBalance = Map.findWithDefault 0 acc blns in 
                         Map.insert acc (oldBalance + diff) blns

-- added -(fee tx) to sender balance
-- the order of arguments is changed to simplify later foldl's
applyTx :: Map.Map Account Int -> Transaction  -> Map.Map Account Int
applyTx blns tx = addMoney amt (recipient tx) $ addMoney (-amt-(fee tx)) (sender tx) blns
    where
        amt = amount tx

processBlock :: Block -> Map.Map Account Int -> Map.Map Account Int
processBlock block priorBalances = appliedWithFees
    where
        txs = transactions block
        txApplied = foldl applyTx priorBalances txs
        fees = sum (map fee txs)
        appliedWithFees = addMoney fees (generator block) txApplied

-- the order of arguments is changed to simplify later foldl's
-- changed (++) to (:) to save order of txs
pushBlock :: Node -> Block -> Block -> Node
pushBlock node pb bl = let view = localView node in
      if Map.notMember bl (blockTree view) then        
        let updView = updateView pb bl view in
        --------------------------------------------------
        -- do it more elegant
        let opb = addSortedBlock bl (openBlocks node) in
        let bb' =  head opb in
        let oldbb = bestBlock node in
        let bb = if (totalDifficulty bb' >= totalDifficulty oldbb) then bb' else oldbb in
        node {localView = updView, pendingBlocks = (pb,bl):(pendingBlocks node), 
                                   openBlocks = opb, 
                                   bestBlock = bb}       
      else node                  

pushBlocks :: Node -> [(Block, Block)] -> Node
pushBlocks = foldl (\n (pb,b) -> pushBlock n pb b)

data Node =
    Node {
        localView :: LocalView,
        -- renamed to exclude inappropriate usage
        pendingTxs :: [Transaction],
        openBlocks :: [Block],
        pendingBlocks :: [(Block,Block)],
        -- maybe move to view?
        bestBlock :: Block,
        account :: Account -- simplification - one account per node
        --isForging :: Bool - simplification - always on for now
    }  deriving (Show)

nodeId :: Node -> Int
nodeId node = fromIntegral $ first8bytesAsNumber $ publicKey $ account node

instance Eq Node where n1 == n2  = nodeId n1 == nodeId n2
instance Ord Node where compare n1 n2 = compare (nodeId n1) (nodeId n2)

--lastNodeBlock :: Node -> Block
--lastNodeBlock nd = last $ nodeChain $ localView nd

--nodeChainLength :: Node -> Int
--nodeChainLength nd = length $ nodeChain $ localView nd

processIncomingBlock :: Node -> Block -> Block -> Node
processIncomingBlock node pb block = updNode
    where
        sigHash = first8bytesAsNumber $ calcGenerationSignature pb (generator block)
        -- todo: many other checks!!
        updNode = case first8bytesAsNumber (generationSignature block) == sigHash of
            True -> pushBlock node pb block
            False -> node

selfBalance :: Node -> Int
selfBalance node = let v = localView node in 
                   accountBalance v (bestBlock node) (account node)


calculateHit :: Block -> Account -> Integer
calculateHit prevBlock acc = fromIntegral $ first8bytesAsNumber $ calcGenerationSignature prevBlock acc


-- hitTime :: Account -> Block -> LocalView -> Timestamp
-- hitTime acct prevBl view = (blockTimestamp prevBl) + (calculateHit prevBl acct)*(effectiveBalance view acct) `div` (baseTarget prevBl)


verifyHit :: Integer -> Block -> Timestamp -> Int -> Bool
verifyHit hit prevBlock timestamp effBalance = (eta > 0) && hit < target -- && hit >= prevTarget) - after block 215000
    where eta = timestamp - blockTimestamp prevBlock
          effbt = (toInteger effBalance)*(baseTarget prevBlock)
          target = effbt*(toInteger eta)
          -- prevTarget = effbt * (eta-1)

-- what is better (>) or (>=)?
addSortedBlock ::  Block -> [Block] -> [Block]
addSortedBlock b [] = [b]
addSortedBlock b lb@(b':bs) =  if ((totalDifficulty b) >= (totalDifficulty b')) then b:lb
                                                           else b':(addSortedBlock b bs)


forgeBlock :: Block -> Node -> Timestamp -> Node
forgeBlock pb node ts = 
   let view = localView node in
   let prTxs = Map.findWithDefault [] pb $ blockTransactions view in 
   let txs = filter (\tx -> not $ elem tx prTxs) $ pendingTxs node in 
   let acct = account node in 
   let effb = effectiveBalance view pb acct in
   let hit = calculateHit pb acct in
   let checkHit = verifyHit hit pb ts effb in
   let openb = openBlocks node in
   if checkHit then let newb = formBlock pb acct ts txs in                    
                        pushBlock node pb newb
               else node {openBlocks = addSortedBlock pb openb}
                    
                    
splitBlocks :: Int -> [Block]  -> ([Block], [Block])
splitBlocks k lb  | k < 0 = (lb, [])
--splitBlocks 0 lb  = (defl, [])
splitBlocks k lb  | k >= 0 = splitAt k lb
        
forgeBlocks ::  Timestamp -> Node -> Node          
forgeBlocks ts node = let acc = account node in                    
                      let view = localView node in
                      let opb = openBlocks node in                      
                      let (blocks, rb) = splitBlocks (tfdepth acc) opb in                             
                      let node' = node {openBlocks = []} in
                      foldl (\n pb -> forgeBlock pb n ts) node' blocks
                      
treeChain :: Block -> BlockTree -> BlockChain                     
treeChain b t = if Map.member b t then 
                  let pb = Map.findWithDefault b b t in
                           (treeChain pb t) ++ [b]
                else [b]
                                                           
                      
bestChain :: Node -> BlockChain
bestChain node = let view = localView node in
                 let tree = blockTree view in
                 treeChain (bestBlock node) tree
              
            
-- removed accumulator common to reduce (++) operations
commonChain :: BlockChain -> BlockChain -> BlockChain
commonChain chain1 chain2 = case (chain1, chain2) of
        (bl1:ct1, bl2:ct2) -> if bl1 == bl2 then bl1:(commonChain ct1 ct2) else []
        _ -> []


data Network =
    Network {
        nodes :: [Node],
        connections :: Map.Map Node [Node] -- todo add latency(avg latency time), trust?
    }  deriving (Show)


outgoingConnections :: Network -> Node -> [Node]
outgoingConnections system node = Map.findWithDefault [] node (connections system)


updateNode :: Node -> Network -> Network
updateNode nd network = network {nodes = ns}
    where
      -- (==) for nodes as Ids 
        ns = map (\n -> if (n == nd) then nd else n) (nodes network)


--blockTree :: Network -> BlockTree
--blockTree sys = error "not impl"

-- todo: define canonical blockchain and implement it's extraction from blocktree of a system
canonicalBlockchain :: Network -> Maybe BlockChain
canonicalBlockchain sys = Nothing