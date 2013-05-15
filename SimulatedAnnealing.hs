module SimulatedAnnealing where

import Parameters (cutoff)
import Positions (ptStart)
import ScoreTab (Score, scoreTabDelta, scoreTabDeltaRelative, scoreTab)
import Tablature (Tab, updatePosContext)
import TabGeneration (emptyPC, suitablePositions, continuesPosition)
import Tuning (Tuning)

import qualified Data.Map as M
import Data.List (maximumBy)
import System.Random (RandomGen, StdGen, mkStdGen, random)
import Data.Ord (comparing)

data SAState = SA { 
  saTab :: Tab, 
  saTuning :: Tuning,
  saNextPos :: Int,
  saScore :: Score,
  saRandGen :: StdGen,
  saValid :: Bool
} deriving (Show)

--
--
--Simulated Annealing functions
--
--

--Gives the probability of a score x at inverse temperature beta.
prob :: Double -> Double -> Double
prob beta x = exp (beta*x)


--convert a list of elements to a PDF given a potential function (score) and an
--inverse temperature (beta).
scoreToPDF :: Double -> (a -> Score) -> [a] -> [(a, Score)]
scoreToPDF beta scorer ls = let total = foldr1 (+) $ map (prob beta.scorer) ls
                          in [( x , ((prob beta.scorer) x)/total )  | x <- ls ]


--Given a random number a between 0 and 1, outputs an element from a PDF
selectRand :: (Fractional a, Ord a) => a -> [(b, a)] -> b
selectRand x [] = error "Empty list to select from"
selectRand x ((v,p):vs) = if x < p then v else selectRand (x-p) vs


--Performs one step of simulated annealing/Monte-Carlo Markov chain
mcmcStep :: Double -> SAState -> SAState
mcmcStep beta (SA tab1 tun n1 score1 rand1 valid1) = 
	let prevPC = if n1==0 then emptyPC else snd (M.elemAt (n1-1) tab1)
            (t, thisPC) = M.elemAt n1 tab1
            suitable = suitablePositions tun (updatePosContext t prevPC) (M.elems $ M.filter (\pt -> t==(ptStart pt)) (snd thisPC))
            getScore pos = scoreTabDeltaRelative tab1 (t, pos)
            (posr, rand2) = random rand1
            newPos = selectRand posr (scoreToPDF beta getScore (take cutoff suitable))
            delta = scoreTabDelta tab1 (t, newPos)
            tab2 = M.updateAt (\ _ _ -> Just newPos) n1 tab1
            score2 = score1+delta
            nextSuitable = n1==M.size tab1 || continuesPosition (updatePosContext t thisPC) (fst $ snd $ M.elemAt (n1+1) tab2)
            updown :: Double
            (updown, rand3) = random rand2
            n2 = (if updown<(1/3) && nextSuitable then n1-1 else n1+1) `mod` (M.size tab1)
        in SA tab2 tun n2 score2 rand3 nextSuitable


--An example temperature profile for simulated annealing. The inverse temperature increases
--linearly with step size in this example.
tempProfile :: Int -> Double
tempProfile n = (fromIntegral n)/100


--Performs simulated annealing infinitely many times, using a given temperature profile,
--and collects the results into an infinite list.
mcmcAll :: Tab -> Tuning -> (Int -> Double) -> StdGen -> [SAState]
mcmcAll tab tun beta rand = 
	let score = scoreTab tab
	    loc = 0
	    first = SA tab tun loc score rand True
            n = 0
            next num el = mcmcStep (beta num) el
            stepList num el = next num el : stepList (num+(if saValid (next num el) then 1 else 0)) (next num el)
	in filter saValid $ stepList 0 first


--Selects the optimal tab from a list of Simulated Annealing results.
mcmcBest :: Int -> [SAState] -> Tab
mcmcBest n tabs = saTab $ maximumBy (comparing saScore) $ take n $ tabs


