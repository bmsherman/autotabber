module Positions where
import Euterpea (Pitch, PTime, trans)
import qualified Data.Map as M
import Data.Maybe (catMaybes)

class (Bounded a, Enum a) => Finite a where
  each :: [a]
  each = [minBound..maxBound]

data Finger = F1 | F2 | F3 | F4 | F5 deriving (Bounded, Enum, Show, Eq, Ord)
instance Finite Finger where
  each = [minBound..maxBound]

data GString = S1 | S2 | S3 | S4 | S5 | S6 deriving (Bounded, Enum, Show, Eq, Ord)
instance Finite GString where
  each = [minBound..maxBound]

data Fingering = Bar GString GString Fret | On GString Fret deriving (Show, Eq)

type Fret = Int

type Position = M.Map Finger Fingering

type PitchT = (Pitch, PTime, PTime)
type StruckNotes = M.Map GString PitchT



--Functions for accessing information from PitchT, PosContext, etc. values
ptPitch :: PitchT -> Pitch
ptPitch (pitch,_,_) = pitch

ptStart, ptEnd :: PitchT -> PTime
ptStart (_,start,_) = start
ptEnd   (_,_,  end) = end



--
--Guitar Tablature Functions
--

--Fingering Functions

getFret :: Fingering -> Fret
getFret (On _ f)    = f
getFret (Bar _ _ f) = f

mapFinger :: (Fret -> Fret) -> Fingering -> Fingering
mapFinger f (On s x)      = On s (f x)
mapFinger f (Bar s1 s2 x) = Bar s1 s2 (f x)


--Returns the fret that a given guitar string would be pressed on
--given a single fingering
fingerFret :: GString -> Fingering -> Fret
fingerFret x (On s f)      = if x==s then f else 0
fingerFret x (Bar s1 s2 f) = if x `elem` [s1..s2] then f else 0

isBar :: Fingering -> Bool
isBar (Bar _ _ _) = True
isBar _           = False

--
--Position Functions
--


position :: [(Finger, Fingering)] -> M.Map Finger Fingering
position = M.fromList

--Given a position, returns the frets at which each string is pressed.
frets :: Position -> M.Map GString Fret
frets p = M.fromList $ [(s, snd $ maxFretFinger p s) | s <- each::[GString] ] 


--Returns the (key,value) pair corresponding to the maximum value in the map
--or Nothing if the map is null
mapMax :: (Ord k, Ord a,Num a) => M.Map k a -> (Maybe k,a)
mapMax = M.foldrWithKey (\k1 x1 (mk2,x2)-> if x2 >= x1 then (mk2,x2) else (Just k1,x1)) (Nothing, 0)


--Given a position and a string, returns either Just the finger that is pressing
--on the string at the highest point (and the corresponding fret)
--or Nothing and 0 (open string).
maxFretFinger :: Position -> GString -> (Maybe Finger, Fret)
maxFretFinger pos str = mapMax $ M.map (fingerFret str) pos


--Given some notes that have been struck and a position, returns
--the fingerings that must still be held in order to hold those notes.
requiredPos :: StruckNotes -> Position -> Position
requiredPos strs pos = let fingers = catMaybes $ map (fst.(maxFretFinger pos)) $ M.keys strs
                       in M.filterWithKey (\k _ -> k `elem` fingers) pos

--Moves a position up or down the fretboard.
transPos :: Int -> Position -> Position
transPos x = M.map $ mapFinger (+x)

--map a function on strings to all strings in a position
mapString :: (GString -> GString) -> Fingering -> Fingering
mapString f (Bar s1 s2 x) = Bar (f s1) (f s2) x
mapString f (On s x)      = On (f s) x

