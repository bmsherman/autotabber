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
halfStepDown = M.map (trans (-1)) stdTuning
fullStepDown = M.map (trans (-2)) stdTuning
dropDTuning = M.adjust (const (D,3)) S1 stdTuning
doubleDropD = M.adjust (const (D,5)) S6 dropDTuning
openG6Tuning = M.adjust (const (G,3)) S2 dropDTuning
openCTuning = tuning [(C,3), (G,3), (C,4), (G,4), (C,5), (E,5)]
dadgadTuning = tuning [(D,3), (A,3), (D,4), (G,4), (A,4), (D,5)]



tuningStrings = 
       [("Standard", stdTuning),
        ("HalfStepDown", halfStepDown),
        ("FullStepDown", fullStepDown),
	("DropD", dropDTuning),
	("DoubleDropD", doubleDropD),
	("OpenG6", openG6Tuning),
	("OpenC", openCTuning),
	("DADGAD", dadgadTuning)]

