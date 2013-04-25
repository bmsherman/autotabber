module Tablature where
import Positions

import Euterpea
import qualified Data.Map as M
import Data.List (groupBy)



type NoteSet = [(PTime, [PitchT])]


type PosContext = (Position, StruckNotes)

type Tab = M.Map PTime PosContext


--for Tab values
tPos = fst



--
--Performance to Note sets
--

defPerform1 :: Music1 -> Performance
defPerform1 = perform defPMap defCon

defPerform :: Music Pitch -> Performance
defPerform = defPerform1.toMusic1

--Filters out only notes that are being played at a given time
playingAtTime :: PTime -> Performance -> Performance
playingAtTime t events = filter ( \e -> (t >= (eTime e) && t < (eTime e + eDur e) ) ) events

--Converts Performance datatype to NoteSet datatype
toNoteSet :: Performance -> NoteSet
toNoteSet events = map (\l -> (fst $ head l, map snd l)) $
	groupBy (\x y -> fst x==fst y) [(eTime e, (pitch $ ePitch e, eTime e, eTime e + eDur e)) | e <- events]




updatePosContext :: PTime -> PosContext -> PosContext
updatePosContext t (pos, pitchMap) = let newPitches = M.filter ((>t).ptEnd) pitchMap
                                     in (requiredPos newPitches pos , newPitches)
