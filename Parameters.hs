module Parameters where

--
--Fixed parameters that could be changed
--

--speedFunc weighs changes that occur after shorter time intervals more heavily than after long intervals
speedFunc :: Fractional a => Rational -> a
speedFunc t = (1/4)/(fromRational t)


--Weighs how much importance (in scoring) is given to changes in positions, relative to how
--how much importance is given to the comfort of single positions in isolation
changeImportance = 2 :: Double


--For simulated annealing, a single Position in a tab is mutated at a time. The mutation
--is chosen from a PDF of possible positions that could replace it. This cutoff limits
--the choosing of possible positions to the (cutoff) best individually-scoring positions.
--This makes the computations much faster (since we don't need to compute a TabDelta
--for tons of different positions) but could end up consistently missing optimal
--conformations.
cutoff = 100 :: Int



--The number of possible start/end positions to keep track of while doing the dynamic
--programming algorithm. It takes the best positions according to their individual score.
numtracks = 30 :: Int

--For the brute force solution to the "chunks" inside the dynamic programming algorithm,
--these factors prevent the "combinatorial explosion" from getting too large. For each
--start point, it only allows prunestart many descendants on the tree of possible tablatures,
--and each descendent at level k is only allowed prunestart*prunefactor^k possible tablatures.
prunestart = 10 :: Int
prunefactor = 1/2
