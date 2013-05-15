module Main where
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
import System.Environment


main = do
   (filename:tuningstr:nstr:accstr:measstr:rest) <- getArgs
   music <- midi filename
   let tuning = select (error "Invalid Tuning") tuningStrings tuningstr
   let (tab, score) = complete tuning (defPerform1 music)
   tabOutput (readInt nstr) (readRat accstr) (readRat measstr) tab
   



--General use functions

--pretty print a list
pp :: Show a => [a] -> IO()
pp = mapM_ print

readInt :: (Num a, Read a) => String -> a
readInt = fst.head.reads

readRat :: String -> Rational
readRat = fst.head.reads



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
------Convenient functions and examples
----
--

--import a MIDI file into a Euterpea Music format
midi :: String -> IO Music1
midi = tcd 

--Return a tab (using the dynamic algorithm) for a MIDI file for a given tuning
midiToTab :: String -> Tuning -> IO Tab
midiToTab filename tuning = midi filename >>= return . dynamic tuning . defPerform1

--Run a standard Simulated Annealing run from a starting tab, and return
--an infinite list of simulated annealing results.
stdmcmcAll :: Tab -> Tuning -> IO [SAState]
stdmcmcAll tab tuning = getStdGen >>= return . mcmcAll tab tuning tempProfile




--Examples for John Butler Trio's Mist song:

mistTab :: IO Tab
mistTab = midiToTab "midi/mist.mid" dropDTuning

mistTabOutput :: Tab -> IO ()
mistTabOutput = lineByLine . annotatedTab 3 (1/8) 3

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


