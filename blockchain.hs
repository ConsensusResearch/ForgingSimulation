module Blockchain.Structures where
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.ByteString.Lazy as B
import qualified Data.Binary.Get as BI
import qualified Data.Map as Map

-- Account-based proof-of-stake cryptocurrency model

type Timestamp = Int

first8bytesAsNumber :: B.ByteString -> Integer
first8bytesAsNumber bs =  fromIntegral $ BI.runGet BI.getWord64le first8 where first8 = B.take 8 bs


data Account =
    Account {
        publicKey :: B.ByteString
    }

instance Show Account where show acc = show $ accountId acc
instance Eq Account where a1 == a2  = accountId a1 == accountId a2
instance Ord Account where compare a b = compare (accountId a) (accountId b)


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

instance Eq Transaction where t1 == t2  = accountId (sender t1) == accountId (sender t2)
                                            && accountId (recipient t1) == accountId (recipient t2)
                                            && txTimestamp t1 == txTimestamp t2

validate :: Transaction -> Bool
validate _ = True

minFee :: Int
minFee = 1



data Block =
    Block {
        transactions :: [Transaction],
        baseTarget :: Integer,
        generator :: Account,
        generationSignature :: B.ByteString,
        blockTimestamp :: Timestamp
    }  deriving (Show)


initialBaseTarget :: Integer
initialBaseTarget = 153722867


maxBaseTarget :: Integer
maxBaseTarget = initialBaseTarget * (fromIntegral systemBalance)

containsTx :: Transaction -> Block -> Bool
containsTx tx block = elem tx $ transactions block


calcGenerationSignature :: Block -> Account -> B.ByteString
calcGenerationSignature prevBlock acct = SHA.bytestringDigest $ SHA.sha256 $ B.append (generationSignature prevBlock) pk
    where pk = publicKey acct


formBlock :: Block -> Account -> Timestamp -> [Transaction] -> Block
formBlock prevBlock gen timestamp txs =
    Block{transactions = filter validate txs, blockTimestamp = timestamp, baseTarget = bt, generator = gen, generationSignature = gs}
    where prevTarget = baseTarget prevBlock
          maxTarget = min (2*prevTarget)  maxBaseTarget
          minTarget = max (prevTarget `div` 2)  1
          candidate = prevTarget*(toInteger (timestamp - (blockTimestamp prevBlock))) `div` 60
          bt = min (max minTarget candidate) maxTarget
          gs = calcGenerationSignature prevBlock gen -- is gs 64 bytes?

blockId :: Block -> Int
blockId block = (blockTimestamp block) + (fromIntegral $ first8bytesAsNumber $ generationSignature block)






type BlockChain = [Block]

two64 :: Double
two64 = 18446744073709551616.0

cumulativeDifficulty :: BlockChain -> Double
cumulativeDifficulty chain = sum(map (\bl -> two64 / (fromIntegral $ baseTarget bl) ) chain)





type BlockTree = [BlockChain]




data LocalView =
    LocalView{
        nodeChain :: BlockChain, -- simplification - one blockchain per node(but true for NRS)
        balances :: Map.Map Account Int
    } deriving (Show)



accountBalance :: LocalView -> Account -> Int
accountBalance view acc = Map.findWithDefault 0 acc (balances view)


effectiveBalance :: LocalView -> Account -> Int
effectiveBalance view acc = accountBalance view acc -- todo: simplification, no 1440 blocks waiting for now


addMoney ::  Int -> Account ->  Map.Map Account Int -> Map.Map Account Int
addMoney diff acc blns = Map.insert acc ((Map.findWithDefault 0 acc blns) + diff) blns


applyTx :: Transaction -> Map.Map Account Int -> Map.Map Account Int
applyTx tx blns = addMoney amt (recipient tx) $ addMoney (-amt - (fee tx)) (sender tx) blns
    where
        amt = amount tx


processBlock :: Block -> Map.Map Account Int -> Map.Map Account Int
processBlock block priorBalances = appliedWithFees
    where
        txs = transactions block
        txApplied = foldl (\bs tx -> applyTx tx bs) priorBalances txs
        fees = sum(map fee txs)
        appliedWithFees = addMoney fees (generator block) txApplied


pushBlock :: Block -> Node -> Node
pushBlock bl node = node{localView = updView, unconfirmedTxs = updTxs}
    where
        view = localView node
        txs = unconfirmedTxs node
        updTxs = foldl (\ts tx -> if containsTx tx bl then ts else ts++[tx]) [] txs
        updView = view{nodeChain = nodeChain view ++ [bl], balances = processBlock bl $ balances view}


pushBlocks :: [Block] -> Node -> Node
pushBlocks bls node = foldl (\n bl -> pushBlock bl n) node bls





data Node =
    Node {
        localView :: LocalView,
        unconfirmedTxs :: [Transaction],
        account :: Account -- simplification - one account per node
        --isForging :: Bool - simplification - always on for now
    }  deriving (Show)


instance Eq Node where n1 == n2  = nodeId n1 == nodeId n2
instance Ord Node where compare a b = compare (nodeId a) (nodeId b)

lastNodeBlock :: Node -> Block
lastNodeBlock nd = last $ nodeChain $ localView nd

nodeChainLength :: Node -> Int
nodeChainLength nd = length $ nodeChain $ localView nd


processIncomingBlock :: Block -> Node -> Node
processIncomingBlock block node = updNode
    where
        lastBlock = lastNodeBlock node
        sigHash = first8bytesAsNumber $ calcGenerationSignature lastBlock (generator block)
        updNode = case first8bytesAsNumber (generationSignature block) == sigHash of
            True -> pushBlock block node
            False -> node

nodeId :: Node -> Int
nodeId node = fromIntegral $ first8bytesAsNumber $ publicKey $ account node

selfBalance :: Node -> Int
selfBalance node = accountBalance (localView node) (account node)


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


forge :: Node -> Timestamp -> Maybe Node
forge node ts = case checkhit of
        True -> Just $ pushBlock generatedBlock node
        False -> Nothing
    where
        view = localView node
        acct = account node
        lastbl = last $ nodeChain view
        hit = calculateHit lastbl acct
        effBalance = effectiveBalance view acct
        checkhit = verifyHit hit lastbl ts effBalance
        generatedBlock = formBlock lastbl acct ts (unconfirmedTxs node)







maxBlocksFromPeer :: Int
maxBlocksFromPeer = 10*1440


commonChain :: BlockChain -> BlockChain -> BlockChain -> BlockChain
commonChain chain1 chain2 common = case (chain1, chain2) of
        (bl1:ct1, bl2:ct2) -> if blockId bl1 == blockId bl2 then commonChain ct1 ct2 (common ++ [bl1]) else common
        _ -> common


-- Nodes are modifiyng !!! so map works wrong, changed [Node] to [Int]
data Network =
    Network {
        nodes :: [Node],
        connections :: Map.Map Node [Int] -- todo add latency(avg latency time), trust?
    }  deriving (Show)


systemBalance :: Int
systemBalance = 1000000000

outgoingConnections :: Network -> Node -> [Node]
outgoingConnections network node = let ids = Map.findWithDefault [] node (connections network) in
                                   filter (\n -> elem (nodeId n) ids) (nodes network) 

outgoingConnectionsIds :: Network -> Node -> [Int]
outgoingConnectionsIds network node = Map.findWithDefault [] node (connections network)


updateNode :: Node -> Network -> Network
updateNode nd network = network{nodes = ns}
    where
        ndId = nodeId nd
        ns = map (\n -> if (nodeId n == ndId) then nd else n) (nodes network)


blockTree :: Network -> BlockTree
blockTree sys = error "not impl"

-- todo: define canonical blockchain and implement it's extraction from blocktree of a system
canonicalBlockchain :: Network -> Maybe BlockChain
canonicalBlockchain sys = Nothing