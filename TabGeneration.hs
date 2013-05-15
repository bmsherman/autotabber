module TabGeneration where
import Positions (Position, PitchT, StruckNotes, GString, ptPitch)
import Tablature (PosContext, Tab, NoteSet, updatePosContext, toNoteSet)
import Tuning (Tuning, pitches)
import ScoreTab
import Parameters (numtracks, prunestart, prunefactor)

import Euterpea (PTime, Pitch, Event, eTime, eDur, Performance)
import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (maximumBy, groupBy, sortBy)
import Data.Ord (comparing)

import Control.Parallel.Strategies

data Tree a = Node a [Tree a] deriving Show

type NextTabGenerator = Tuning -> (PTime, PosContext) -> (PTime, [PitchT]) -> (PTime, PosContext)

--
--Functions for playing notes with positions
--

--Given several available guitar strings that are in position to play given pitches,
--and a set of pitches that needs to be played, returns Nothing if it cannot possibly
--play the given pitches.
playNotesWith :: M.Map GString Pitch -> [PitchT] -> Maybe StruckNotes
playNotesWith possPitches (p:ps) = do x <- playNoteWith possPitches p; 
					fmap (M.insert x p) $ playNotesWith possPitches ps
playNotesWith possPitches [] = Just M.empty


playNoteWith :: M.Map GString Pitch -> PitchT -> Maybe GString
playNoteWith possPitches p = let playPitches = (M.keys . M.filter (ptPitch p==)) possPitches
			     in if (null playPitches) then Nothing else Just (maximum playPitches)


--Given a tuning, a PosContext that corresponds to the fingerings and strings that must be left
--alone in order to sustain certain notes, and a set of pitches that needs to be played,
--returns a list of all possible resulting PosContexts that would allow those pitches to be played.
suitablePositions :: Tuning -> PosContext -> [PitchT] -> [PosContext]
suitablePositions t pc ps = let toPlay p = playNotesWith (M.difference (pitches t p) (snd pc)) ps
                                newStruckNotes newMap = M.unionWith (error "Re-striking string") (snd pc) newMap
                                list = catMaybes $ [do newMap <- toPlay pos; Just (pos, newStruckNotes newMap) | pos <- (restrictedPositions pc)]
                            in if null list
                               then error ("No positions for position " ++ show pc ++ ", pitches " ++ show ps)
                               else list


--Gives only those positions which, given a PosContext corresponding to fingerings and strings
--that must be left alone, would not mess up those things that need to be left alone.
restrictedPositions :: PosContext -> [Position]
restrictedPositions pc = filter (continuesPosition pc) allPositions

continuesPosition :: PosContext -> Position -> Bool
continuesPosition pc pos = M.null $ M.filter (id) $ M.intersectionWith (/=) (fst pc) pos











--
--Tab Generation
--

--Empty PosContext - this is used as the "starter" PosContext for a guitar piece
emptyPC :: PosContext
emptyPC = (M.empty, M.empty)


--A function that given a function (NextTabGenerator) that chooses each successive position
--given the tab that has been built so far, and given the tuning, and the NoteSet describing
--the performance, it builds the Tab for the piece.
forwardTab :: NextTabGenerator -> Tuning -> Performance -> Tab
forwardTab nextTabGen t perf = extend (pt1, (selfishPos t pitches1)) (tail noteset) where
	noteset = toNoteSet perf
	extend first@(fstt, fstp)  (p:ps) = M.insert fstt fstp $ extend (nextTabGen t first p) ps
	extend (fstt, fstp) [] = M.singleton fstt fstp
	(pt1, pitches1) = head noteset


--Chooses the best position without regarding any context (only looking at individual position
--score). Used to choose the first tab.
selfishPos :: Tuning -> [PitchT] -> PosContext
selfishPos t ps = let suitable = suitablePositions t emptyPC ps
                     in if null suitable
                     then error $ "No suitable positions for " ++ (show ps)
                     else head suitable


--Given the previous PosContext, chooses the Position for the next set of notes that would
--maximize the score of the tab that has been built so far
smartNext :: NextTabGenerator
smartNext tun (t1, pc) (t2, ps) = (t2, fst $ smartBestNext changeScore firstSuitable suitable)
        where changeScore p = changePosScore (t2-t1) (fst pc) p
              suitable = suitablePositions tun (updatePosContext t2 pc) ps
              firstPos = fst (head suitable)
              firstSuitable = (head suitable, scorePos firstPos + changeScore firstPos)

--Helper function for smartNext.
smartBestNext :: (Position -> Score) -> (PosContext, Score) -> [PosContext] -> (PosContext, Score)
smartBestNext changeScore (pcA, scoreA) [] = (pcA, scoreA)
smartBestNext changeScore (pcA, scoreA) (pcB@(posB,_):list)
	| scorePos posB < scoreA 			= (pcA, scoreA)
	| scorePos posB + changeScore posB < scoreA 	= (pcA, scoreA)
        | otherwise					= smartBestNext changeScore (pcB,scorePos posB + changeScore posB) list



--Chooses the next Position, given the previous PosContext, by choosing the Position that satisfies
--the constraints of the previous PosContext, but takes the maximum only looking at individual scores.
selfishNext :: NextTabGenerator
selfishNext tun (t1, pc) (t2, ps) = (t2, head $ suitablePositions tun (updatePosContext t2 pc) ps)


--Build a tab using selfishNext
selfishTab :: Tuning -> Performance -> Tab
selfishTab = forwardTab selfishNext

--Build a tab using smartNext
smartFTab :: Tuning -> Performance -> Tab
smartFTab = forwardTab smartNext




inside, outside :: Ord a => (a,a) -> (a,a) -> Bool
(a,b)  `inside` (c,d) = c <= a && b <= d
(a,b) `outside` (c,d) = b <= c || a >= d

union :: Ord a => (a,a) -> (a,a) -> (a,a)
(a,b) `union` (c,d) = (a `min` c, b `max` d)

eRange :: Event -> (PTime,PTime)
eRange e = (eTime e, eTime e + eDur e)



--break a Performance into disjoint Performances without overlapping notes
chunks :: Performance -> [Performance]
chunks p = chunksH revp (eRange last) [[last]]
	where (last:revp) = reverse p


--Helper function for the chunks function
chunksH :: Performance -> (PTime, PTime) -> [Performance] -> [Performance]
chunksH [] _ acc = acc
chunksH (e:es) range acc@(cur:rest) = 
	if (eR `inside` range) 
	then chunksH es range ((e:cur):rest)
	else chunksH (e:es) (if new then eR else (eR `union` range)) (if new then ([]:acc) else acc)
	where eR = eRange e
	      new = eR `outside` range

--Prune a tree to have at most n branches, and then make all the children have
--at most n*k branches.
prune :: Int -> Double -> Tree a -> Tree a
prune n k (Node x xs) = Node x (map (prune (ceiling (k*fromIntegral n)) k) (take n xs))

prune2 n (Node x xs)  = prune n 1


--For a given "chunk", generate a list of trees of how the chunk might be played. Each tree
--has a different startpoint.
treeSection :: Tuning -> NoteSet -> [Tree TabDelta]
treeSection tun ((t1, ps1):ns) = 
	let starts = [ (t1,pc) | pc <- suitablePositions tun emptyPC ps1 ]
	    tree [] td = Node td []
	    tree ((t2,ps2):noteset) td = Node td [ tree noteset (t2,pc) | pc <- suitablePositions tun (updatePosContext t2 (snd td)) ps2 ]
	in (map (prune prunestart prunefactor . tree ns) (take numtracks starts))


--Traverse a tree that is output from treeSection to generate a list
--of all the different tablatures that the tree corresponds to.
treeToList :: Ord k => Int -> Tree (k,a) -> [M.Map k a]
treeToList 1 (Node (k,v) xs) = [ M.singleton k v ]
treeToList n (Node (k,v) xs) = map (M.insert k v) $ concatMap (treeToList (n-1)) xs

--Takes a tree of possibilities with the same start point, and returns
--a list of all the best tabs, given different endpoints
smushEnds :: Int -> Tree TabDelta -> [(Tab,Score)]
smushEnds n tree = take numtracks $ map (maximumBy (comparing snd) . map (\x -> (x, scoreTab x)) ) $ list2 
	where list = treeToList n tree
	      list2 = groupBy comp $ sortBy (comparing M.findMax) list
	      comp x y = M.findMax x==M.findMax y

--This is the "base case" of our dynamic programming algorithm. It generates
--a list of lists, where each list in the large list contains different possible
--tablatures with the same start points.
listForChunk :: Tuning -> NoteSet -> [[(Tab,Score)]]
listForChunk tun ns = map (smushEnds (length ns)) $ treeSection tun ns

--Given two lists of possible beginnings and ends, finds the combination of a beginning
--and an end that maximize the total score of the entire tab.
bestConnection :: [(Tab,Score)] -> [(Tab,Score)] -> (Tab,Score)
bestConnection begs ends = maximumBy (comparing snd) [ (\end -> (M.union (fst beg) (fst end), mergeScore beg end) ) $ maximumBy (comparing $ mergeScore beg) ends | beg <- begs ]

--Gives the resulting score that comes from merging two tabs.
mergeScore :: (Tab, Score) -> (Tab, Score) -> Score
mergeScore (beg,s1) (end,s2) = s1 + s2 + maybeChangePosScore (maybeM M.findMax beg) (maybeM M.findMin end)

--This is the "merge" step of the dynamic programming algorithm.
--We start from the beginning, so the "begs" is simply a list of tabs with different endpoints.
merge :: [(Tab,Score)] -> [[(Tab,Score)]] -> [(Tab,Score)]
merge begs allends = [ bestConnection begs ends | ends <- allends] `using` parList rseq


--Builds the tab from dynamic programming.
build :: Tuning -> [NoteSet] -> [(Tab, Score)]
build tun (n:ns) = foldl merge initial $ map (listForChunk tun) ns
	where initial = map (maximumBy (comparing snd)) $ groupBy comp $ sortBy (comparing (M.findMax.fst)) $ concat (listForChunk tun n)
	      comp x y = M.findMax (fst x)==M.findMax (fst y)

--Just performs the "end step" of taking the maximum. Also converts the performance to chunks, as needed.
complete :: Tuning -> Performance -> (Tab,Score)
complete tun perf = maximumBy (comparing snd) $ build tun $ map toNoteSet $ chunks perf

--Just returns the tablature from "complete."
dynamic :: Tuning -> Performance -> Tab
dynamic tun perf = fst $ complete tun perf
