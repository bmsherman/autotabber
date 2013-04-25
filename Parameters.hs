module Parameters where

--
--Fixed parameters that could be changed
--

--Weighs how much importance (in scoring) is given to changes in positions, relative to how
--how much importance is given to the comfort of single positions in isolation
changeImportance :: Double
changeImportance = 1


--For simulated annealing, a single Position in a tab is mutated at a time. The mutation
--is chosen from a PDF of possible positions that could replace it. This cutoff limits
--the choosing of possible positions to the (cutoff) best individually-scoring positions.
--This makes the computations much faster (since we don't need to compute a TabDelta
--for tons of different positions) but could end up consistently missing optimal
--conformations.
cutoff :: Int
cutoff = 100

