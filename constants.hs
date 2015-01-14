module Constants where

minFee :: Int
minFee = 1

goalBlockTime :: Integer
goalBlockTime = 60

maxHit :: Integer
maxHit = 2^64

systemBalance :: Int
systemBalance = 10^9

two64 :: Double
two64 = 2**64

maxBlocksFromPeer :: Int
maxBlocksFromPeer = 10*1440


-- tfdepth > 1 finite multibranch 
--         = 1 singlebranch                                                                              
--         = 0 not forging                                                                               
--         < 0 full multibranch (please do not use - exponential growth) 
tfDepth :: Int
tfDepth = 20