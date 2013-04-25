module Tab where
import Positions
import PositionBank
import Output
import Tablature
import TabGeneration
import ScoreTab
import SimulatedAnnealing
import Tuning

import Euterpea
import qualified Data.Map as M
import Data.Maybe (isJust, fromMaybe)
import Data.List (nub)
import System.Random (StdGen, getStdGen)


--General use functions

--pretty print a list
pp :: Show a => [a] -> IO()
pp = mapM_ print






----Functions for loading MIDI files
----This is NOT my own code, and was taken
----from Prof. Hudak's EuterpeaExamples.lhs

loadMidiFile fn = do
  r <- importFile fn 
  case r of
    Left err -> error err
    Right m  -> return m

tcd file = do
             x <- loadMidiFile file
             return $ (fst3.fromMidi) x

fst3 (a,b,c) = a


----This is the end of the code that I took from Prof. Hudak's EuterpeaExamples.lhs





--
--Chords
--

dChord,cChord :: [Pitch]
dChord = [(A,4), (D, 5), (Fs,5)]

cChord = [(C,4),(E,4),(G,4),(C,5),(E,5)]




--
--Example Music
--


--Twinkle Twinkle Little Star
ttls = let 	l1 = ([c, c, g, g, a, a], g)
		l2 = ([f, f, e, e, d, d], c)
		l3 = ([g, g, f, f, e, e], d)
		mkLine (ls, n) = line (map (\f -> f 4 qn) ls) :+: n 4 hn
       in line $ map mkLine [l1, l2, l3, l3, l1, l2]








--
----
------Examples
----
--


midi :: String -> IO Music1
midi = tcd 

midiToTab :: String -> Tuning -> IO Tab
midiToTab filename tuning = midi filename >>= return . smartFTab tuning . toNoteSet . defPerform1

mistTab :: IO Tab
mistTab = midiToTab "midi/john_butler_trio_mist.mid" dropDTuning

mistTabOutput :: Tab -> IO ()
mistTabOutput = lineByLine . annotatedTab 3 (1/8) 6

stdMistTabOutput :: IO ()
stdMistTabOutput = mistTab >>= mistTabOutput

mistTabPositions :: IO ()
mistTabPositions = mistTab >>= tabPositions

mistMCMC :: Int -> IO Tab
mistMCMC n = do
	mt <- mistTab
	results <- stdmcmcAll mt dropDTuning
	print $ take n $ map (\SA {saNextPos = n, saScore = s} -> (n,s)) results
        return (mcmcBest n results)


stdmcmcAll :: Tab -> Tuning -> IO [SAState]
stdmcmcAll tab tuning = getStdGen >>= return . mcmcAll tab tuning tempProfile
