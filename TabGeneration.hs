module TabGeneration where
import Positions (Position, PitchT, StruckNotes, GString, ptPitch)
import Tablature (PosContext, Tab, NoteSet, updatePosContext)
import Tuning (Tuning, pitches)
import ScoreTab (Score, scorePos, changePosScore, allPositions)

import Euterpea (PTime, Pitch)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

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
forwardTab :: NextTabGenerator ->  Tuning -> NoteSet -> Tab
forwardTab nextTabGen t noteset = extend (pt1, (selfishPos t pitches1)) (tail noteset) where
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
selfishTab :: Tuning -> NoteSet -> Tab
selfishTab = forwardTab selfishNext

--Build a tab using smartNext
smartFTab :: Tuning -> NoteSet -> Tab
smartFTab = forwardTab smartNext

