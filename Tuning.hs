module Tuning where

import Positions
import Euterpea
import qualified Data.Map as M



type Tuning = M.Map GString Pitch


--Gives the pitches that each string would correspond to given a tuning and a position.
pitches :: Tuning -> Position -> M.Map GString Pitch
pitches t pos = M.intersectionWith trans (frets pos) t



--Returns a M.map from each of the elements of a finite set
--to the values in the list
fromListAll xs = M.fromList (zip each xs)

--
--Tuning functions
--

tuning :: [Pitch] -> M.Map GString Pitch
tuning = fromListAll


--
--Tunings
--

stdTuning = tuning [(E,3), (A,3), (D,4), (G,4), (B,4), (E,5)]
dropDTuning = M.adjust (const (D,3)) S1 stdTuning
imyoursTuning = M.adjust (const (G,3)) S2 dropDTuning
openCTuning = tuning [(C,3), (G,3), (C,4), (G,4), (C,5), (E,5)]


