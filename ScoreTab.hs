module ScoreTab where
import Positions 
import Tablature
import PositionBank (allPositionsUnsorted)
import Parameters (changeImportance, speedFunc)

import Euterpea (PTime)
import qualified Data.Map as M
import Data.List (sortBy)
import Data.List as List (find)
import Data.Maybe (fromMaybe, fromJust)
import Data.Ord (comparing)
import Control.Applicative (liftA2)



type Score = Double

type TabDelta = (PTime, PosContext)


--For TabDeltas
tdT = fst
tdPos = tPos.snd
tdScore = tScore.snd

--For Tab
tScore= scorePos.tPos





allPositions = sortBy (flip $ comparing scorePos) allPositionsUnsorted


--Matches an input x against the firsts of the pairs in the list.
--It returns the second of the pair if it finds a match.
--Otherwise, it returns the default def.
select :: Eq a => b -> [(a, b)] -> a -> b
select def xs x = maybe def snd $ List.find (\y -> (fst y)==x) xs


--Applies a list of functions to a given value, and sums the results
scoreFuncs :: Num b =>  [a -> b] -> a -> b
scoreFuncs fs x = foldr (\f acc -> acc + (f x)) 0 fs

--Applies a list of functions to a pair of values, and sums the results
scoreFuncs2 :: Num c =>  [a -> b -> c] -> a -> b -> c
scoreFuncs2 fs x y = foldr (\f acc -> acc + (f x y)) 0 fs

--Returns a map of consecutive pairs of keys, and their corresponding pairs of values
pairWise :: Ord k => M.Map k a -> M.Map (k, k) (a, a)
pairWise map = let ((minK, minX), newMap) = M.deleteFindMin map
               in pairWiseHelper M.empty minK minX newMap

pairWiseHelper acc k x map | M.null map = acc
                           | otherwise  = 
	let ((minK, minX), newMap) = M.deleteFindMin map
	in pairWiseHelper (M.insert (k,minK) (x,minX) acc) minK minX newMap

--Allows me to use functions that fail on null maps by enclosing in the Maybe functor.
maybeM :: Ord k => (M.Map k a -> b) -> M.Map k a -> Maybe b
maybeM f map = if (M.null map) then Nothing else Just (f map)





--
--Large scoring functions
--

changePosScoreTD :: TabDelta -> TabDelta -> Double
changePosScoreTD td1 td2 = changePosScore (tdT td2-tdT td1) (tdPos td1) (tdPos td2)

maybeChangePosScore :: Maybe TabDelta -> Maybe TabDelta -> Double
maybeChangePosScore td1 td2 = fromMaybe 0 $ (liftA2 changePosScoreTD) td1 td2

scoreTab :: Tab -> Double
scoreTab tab = let singleScore = M.fold (+) 0 $ M.map tScore tab
		   posDiff = pairWise $ M.map tPos tab
		   diffScore = M.fold (+) 0 $ M.mapWithKey (\(k1,k2) (p1,p2) -> changePosScore (k2-k1) p1 p2) posDiff
	       in singleScore + diffScore

scoreTabDelta ::  Tab -> TabDelta -> Double
scoreTabDelta tab td2@(t, (pos2, _)) = 
	let tdNew = Just td2
            (lower, td1, upper) = M.splitLookup t tab
            tdOld = Just (t, fromJust td1)
            tdPrev = (maybeM M.findMax) lower
            tdNext = (maybeM M.findMin) upper
	in ((tdScore td2) - (tdScore $ fromJust tdOld)) + maybeChangePosScore tdPrev tdNew - maybeChangePosScore tdPrev tdOld
			                                + maybeChangePosScore tdNew tdNext - maybeChangePosScore tdOld tdNext

scoreTabDeltaRelative :: Tab -> TabDelta -> Double                              
scoreTabDeltaRelative tab td2@(t, (pos2, _)) = 
	let tdNew = Just td2
            (lower, _, upper) = M.splitLookup t tab
            tdPrev = (maybeM M.findMax) lower
            tdNext = (maybeM M.findMin) upper
	in (tdScore td2) + maybeChangePosScore tdPrev tdNew + maybeChangePosScore tdNew tdNext




--
--Scoring Positions
--

--Calculated properties about positions

--Gives where a finger is, usually, in relation to the "hand" on a fretboard.
--I said that "hand" is basically where the index finger is. For example,
--the 4th finger lies about 3/2 frets above the "hand"/index finger in a comfortable situation.
fingerNaturalPos :: (Ord a, Fractional a) => Finger -> a
fingerNaturalPos = select (error "Not a finger?") [(F1,1),(F2,0),(F3,1),(F4,3/2),(F5,2)]

--Calculates where the hand is on the fretboard for a given position.
handPosition :: (Fractional a, Ord a) => Position -> Maybe a
handPosition pos | M.null pos = Nothing
                 | otherwise = Just $ M.fold max (-10) $ M.mapWithKey (\k x -> fromIntegral (getFret x) - fingerNaturalPos k) pos

numFingers :: Position -> Int
numFingers pos = M.size pos

numBars :: Position -> Int
numBars pos = M.size $ M.filter isBar pos


--Individual scoring functions

scorePosFingers :: Fractional a => Position -> a
scorePosFingers pos = (-1) * select (error "Too many fingers!") [(0, 0), (1, 1), (2, 5), (3, 10), (4, 30), (5, 200)] (numFingers pos)

scorePosHandPosition :: (Ord a, Fractional a) => Position -> a
scorePosHandPosition pos = ((-1)/5)*(fromMaybe 0 $ handPosition pos)

scorePosNumBars :: Fractional a => Position -> a
scorePosNumBars pos = (-1) * select (200) [(0,0), (1, 15), (2, 50)] (numBars pos)

scorePosFinger :: Fractional a => Position -> a
scorePosFinger pos = foldr (+) 0 $ map scoreFinger $ M.keys pos


scoreFinger :: Fractional a => Finger -> a
scoreFinger F1 = -50
scoreFinger F2 = 0
scoreFinger F3 = -1/2
scoreFinger F4 = -2
scoreFinger F5 = -10




--Aggregate scoring function


--Given a position, scorePos returns a score that indicates how "comfortable" or easy
--the position is, totally in isolation. Higher scores are better, and no score exceeds 0.
scorePos :: Position -> Double
scorePos = scoreFuncs funcs

funcs = [scorePosFingers,scorePosHandPosition,scorePosNumBars,scorePosFinger]



--
--Differences in Positions
--

--Given an old fingering and a new fingering for the same finger,
--indicates whether the finger simply slides along the string from one fret
--to another, possibly remaining exactly the same.
isSlideFinger :: Fingering -> Fingering -> Bool
isSlideFinger (Bar s1 s2 _) (Bar t1 t2 _) = s1==t1 && s2==t2
isSlideFinger (On s _)      (On t _)      = s==t
isSlideFinger  _              _           = False

--Given two positions, determines whether one is simply a translate of the other.
isSlide :: Position -> Position -> Bool
isSlide pos1 pos2 = M.fold (&&) True $ M.intersectionWith isSlideFinger pos1 pos2

--If a change in position does correspond to sliding fingers, indicates the maximum difference
--in the displacements that each finger must slide
slideVar :: Position -> Position -> Int
slideVar pos1 pos2 = let vars = M.intersectionWith (-) (M.map getFret pos1) (M.map getFret pos2)
		     in if (M.null vars) then 0 else (M.fold max 0 vars) - (M.fold min 22 vars)


--Given a property of fingerings, and an old and a new position, returns how many times
--a new fingering with that property is added.
--addProp :: (Fingering -> Fingering -> Bool) -> Position -> Position -> Int
--addProp f pos1 pos2 = let diffMap = M.differenceWith (\x y -> if x /= y then Just y else Nothing) pos2 pos1
--                      in M.size $ M.filter id $ M.intersectionWith f diffMap pos2

--How many new fingers are made to press on the fretboard, not including slides
addFingers :: Position -> Position -> Int
addFingers pos1 pos2 = M.size $ M.differenceWith f pos2 pos1
	where f y x = if isSlideFinger x y then Nothing else Just y

--How many fingers are removed from the fretboard.
removeFingers :: Position -> Position -> Int
removeFingers = flip addFingers

--How many new bars are made, not including slides.
addBars :: Position -> Position -> Int
addBars pos1 pos2 = M.size $ M.filter isBar $ M.differenceWith f pos2 pos1
	where f y x = if isSlideFinger x y then Nothing else Just y

--How many bars are removed.
removeBars :: Position -> Position -> Int
removeBars = flip addBars


--Scores for these various difference in properites
addFingersScore p1 p2 = (-1)*(select (error "Fingers") [(0, 0), (1, 1), (2, 5), (3, 10), (4, 30), (5, 200)] $ addFingers p1 p2)
removeFingersScore p1 p2 = (-1/4)*(select (error "Fingers") [(0, 0), (1, 1), (2, 5), (3, 10), (4, 30), (5, 200)] $ removeFingers p1 p2)
addBarsScore p1 p2 = (-1)*(select 1000 [(0, 0), (1, 20), (2, 50)] $ addBars p1 p2)
removeBarsScore p1 p2 = (-1)*(select 50 [(0, 0), (1, 5), (2, 15)] $ removeBars p1 p2)

--Score that penalizes making the hand move too far up or down the fretboard.
changeHandPositionScore :: Position -> Position -> Double
changeHandPositionScore p1 p2 =(-3)*(fromMaybe 0 $ fmap abs $ liftA2 (-) (handPosition p1) (handPosition p2))

changeFuncs = [addFingersScore, removeFingersScore, addBarsScore, removeBarsScore, changeHandPositionScore]


--changePosScore scores how difficult it is to chane from the first position given to the second.
--Higher scores are better, and the best score is 0
changePosScore :: PTime -> Position -> Position -> Double
changePosScore t p1 p2 = changeImportance*(speedFunc t)*scoreFuncs2 changeFuncs p1 p2
