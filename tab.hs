module Tab where
import Euterpea
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, fromMaybe, fromJust, listToMaybe)
import Data.List as List (find)
import Data.List (sortBy, nub, maximumBy, groupBy)
import Data.Ord (comparing)
import Control.Applicative ((<|>), liftA2)
import System.Random


--Data Type Declarations

class (Bounded a, Enum a) => Finite a where
  each :: [a]
  each = [minBound..maxBound]

data GString = S1 | S2 | S3 | S4 | S5 | S6 deriving (Bounded, Enum, Show, Eq, Ord)

instance Finite GString where
  each = [minBound..maxBound]

data Finger = F1 | F2 | F3 | F4 | F5 deriving (Bounded, Enum, Show, Eq, Ord)
instance Finite Finger where
  each = [minBound..maxBound]

data Fingering = Bar GString GString Fret | On GString Fret deriving (Show, Eq)

type Fret = Int

type Position = M.Map Finger Fingering

type Tuning = M.Map GString Pitch

type NoteSet = [(PTime, [PitchT])]


type StruckNotes = M.Map GString PitchT
type PosContext = (Position, StruckNotes)
type PitchT = (Pitch, PTime, PTime)

type Score = Double

type Tab = M.Map PTime PosContext
type TabDelta = (PTime, PosContext)

type NextTabGenerator = Tuning -> (PTime, PosContext) -> (PTime, [PitchT]) -> (PTime, PosContext)



--
--Fixed parameters that could be changed
--

--Weighs how much importance (in scoring) is given to changes in positions, relative to how
--how much importance is given to the comfort of single positions in isolation
changeImportance = 1


--For simulated annealing, a single Position in a tab is mutated at a time. The mutation
--is chosen from a PDF of possible positions that could replace it. This cutoff limits
--the choosing of possible positions to the (cutoff) best individually-scoring positions.
--This makes the computations much faster (since we don't need to compute a TabDelta
--for tons of different positions) but could end up consistently missing optimal
--conformations.
cutoff = 100000




--General use functions

--pretty print a list
pp :: Show a => [a] -> IO()
pp = mapM_ print

--Matches an input x against the firsts of the pairs in the list.
--It returns the second of the pair if it finds a match.
--Otherwise, it returns the default def.
select :: Eq a => b -> [(a, b)] -> a -> b
select def xs x = maybe def snd $ List.find (\y -> (fst y)==x) xs

--Or of two Nothings is Nothing
--Or of a Nothing and a Just value is the Just value
--Or of two Just values returns an error
disjointOr :: Maybe a -> Maybe a -> Maybe a
disjointOr (Just _) (Just _) = error "disjointOr: Both values are just!"
disjointOr a b = a <|> b


--Returns a list of all sublists (with order maintained) of that list
--that are of length n.
choose :: Int -> [a] -> [[a]]
choose n xs | n < 0 = error "Must choose positive number."
choose 0 xs = [[]]
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
choose n [] = []

--Returns a M.map from each of the elements of a finite set
--to the values in the list
fromListAll xs = M.fromList (zip each xs)

--Self-explanatory. Used to take maxima/minima of lists of maybes.
maybeF :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeF f (Just x) (Just y) = Just (f x y)
maybeF _  Nothing (Just y) = Just y
maybeF _ (Just x) Nothing  = Just x
maybeF _  Nothing Nothing  = Nothing

--Applies a list of functions to a given value, and sums the results
scoreFuncs :: Num b =>  [a -> b] -> a -> b
scoreFuncs fs x = foldr (\f acc -> acc + (f x)) 0 fs

--Applies a list of functions to a pair of values, and sums the results
scoreFuncs2 :: Num c =>  [a -> b -> c] -> a -> b -> c
scoreFuncs2 fs x y = foldr (\f acc -> acc + (f x y)) 0 fs

--Splits a map, and inserts the middle value (if it exists)
--into the upper map.
splitU :: Ord k => k -> M.Map k a -> (M.Map k a, M.Map k a)
splitU k map = let (l,m,u) = M.splitLookup k map
                   newu = fromMaybe u $ fmap (\x-> M.insert k x u) m
                   in (l, newu)

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


--Functions for accessing information from PitchT, PosContext, etc. values
ptPitch :: PitchT -> Pitch
ptPitch (pitch,_,_) = pitch

ptStart, ptEnd :: PitchT -> PTime
ptStart (_,start,_) = start
ptEnd   (_,_,  end) = end

--For TabDeltas
tdT = fst
tdPos = tPos.snd
tdScore = tScore.snd

--for Tab values
tPos = fst
tScore= scorePos.tPos


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


--Gives the pitches that each string would correspond to given a tuning and a position.
pitches :: Tuning -> Position -> M.Map GString Pitch
pitches t pos = M.intersectionWith trans (frets pos) t


--
--Tuning functions
--

tuning :: [Pitch] -> M.Map GString Pitch
tuning = fromListAll

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
addProp :: (Fingering -> Bool) -> Position -> Position -> Int
addProp f pos1 pos2 = let diffMap = M.differenceWith (\x y -> if x /= y then Just x else Nothing) pos2 pos1
                      in M.size $ M.filter id $ M.intersectionWith (\_ y -> f y) diffMap pos2

--How many new fingers are made to press on the fretboard.
addFingers :: Position -> Position -> Int
addFingers = addProp (const True)

--How many fingers are removed from the fretboard.
removeFingers :: Position -> Position -> Int
removeFingers = flip addFingers

--How many new bars are made.
addBars :: Position -> Position -> Int
addBars = addProp isBar

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
changeHandPositionScore p1 p2 =(-1)*(fromMaybe 0 $ fmap abs $ liftA2 (-) (handPosition p1) (handPosition p2))

changeFuncs = [addFingersScore, removeFingersScore, addBarsScore, removeBarsScore, changeHandPositionScore]


--changePosScore scores how difficult it is to chane from the first position given to the second.
--Higher scores are better, and the best score is 0
changePosScore :: PTime -> Position -> Position -> Double
changePosScore t p1 p2 = changeImportance*(speedFunc t)*scoreFuncs2 changeFuncs p1 p2

--speedFunc weighs changes that occur after shorter time intervals more heavily than after long intervals
speedFunc :: Fractional a => PTime -> a
speedFunc t = (1/4)/(fromRational t)

















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




updatePosContext :: PTime -> PosContext -> PosContext
updatePosContext t (pos, pitchMap) = let newPitches = M.filter ((>t).ptEnd) pitchMap
                                     in (requiredPos newPitches pos , newPitches)



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
--Tablature output functions
--


--Given time at which the tab starts, the "bin width" (acc) of time for which each vertical line in the tab should
--represent (the "resolution" of the tab) and the numbe
asciiTab :: PTime -> PTime -> PTime -> Tab -> [String]
asciiTab start acc meas tab = let t = start + acc
                                  (lower,upper) = splitU t tab
                                  firstToUse = map snd $ M.toList lower
                                  nextThing = if (M.null upper) then take 6 $ repeat [] else asciiTab t acc meas upper
                              in zipWith (++) (tabFor start acc firstToUse) nextThing


--Reverses the list of strings to put highest pitch strings on top, and outputs them.
putTab :: [String] -> IO ()
putTab = putStr.unlines.reverse


--Gives a single vertical bin/bar of tab, given the start of that window (start) and the time-width (acc),
--and the PosContext to be played during that time.
tabFor :: PTime -> PTime -> [PosContext] -> [String]
tabFor start acc pcs = let try str (po,pi) = if (M.member str $ M.filter timefilter pi) then Just (snd (maxFretFinger po str)) else Nothing 
                           timefilter pitcht = ptStart pitcht>=start && ptStart pitcht<(start+acc)
                           unannotated s = (twoDnum.listToMaybe.catMaybes.map (try s)) pcs
               in map unannotated (each::[GString])



--Formats tab, and splits it into lines, and adds measure bars.
annotatedTab :: Int-> PTime -> PTime -> PTime -> Tab -> [[String]]
annotatedTab n start acc meas tab = chunks ((round(meas/acc)+1)*n) $ addInfo acc meas $ asciiTab start acc meas $ tab


--Breaks a list of lists into chunks of size n.
chunks :: Int -> [[a]] -> [[[a]]]
chunks n ls | null (head ls) = []
chunks n ls | otherwise      = map (take n) ls : chunks n (map (drop n) ls)


--Formats a fret (or no fret) for a tab
twoDnum :: Maybe Fret -> String
twoDnum (Just n) | n < 10    = '-' : show n
                 | otherwise = show n
twoDnum Nothing              = "--"


--"Intersperse" one string s within another str every n characters, at an offset of i.
addStr :: String -> Int -> Int -> String -> String
addStr s n 0 str = s++(addStr s n n str)
addStr s n i [] = []
addStr s n i (c:str) = c:(addStr s n (i-1) str)


--Add measure information to tablature output
addInfo :: PTime -> PTime -> [String] -> [String]
addInfo acc meas ls = let n = round (meas/acc) in 
                   [addStr "|" n 0 l | l <- ls]


--Properly output multi-line tab
putsTab :: Int -> [String] -> IO ()
putsTab n strs | null (head strs) = return ()
               | otherwise = putTab (map (take n) strs) >> putStr "\n\n" >> putsTab n (map (drop n) strs) 




--Display the hand conformation of a Position using ASCII
asciiPos :: Position -> [String]
asciiPos pos = let fretsFor str = M.map (fingerFret str) pos
                   fingerMap str n = M.filter (==n) (fretsFor str) 
                   disp str n = if (M.null $ fingerMap str n) then '-' else ((head.show.(+1).fromEnum.fst) $ M.findMin (fingerMap str n))
                   display str = [ disp str n | n <- [1..22] ]
               in [ addStr "|" 3 0 $ addStr "-" 2 2 $ addStr "-" 1 0 $ display str | str <- each::[GString] ]



putasciiPos :: Position -> IO()
putasciiPos pos = lineByLine [asciiPos pos]


--list all of the positions in a tab
posInTab :: Tab -> [[String]]
posInTab ls = M.elems $ M.map (asciiPos.tPos) ls

--output all the positions in ASCII format
tabPositions :: Tab -> IO ()
tabPositions = lineByLine.posInTab


--Output a list of strings line by line, with line spaces in between
lineByLine :: [[String]] -> IO ()
lineByLine [] = return ()
lineByLine (l:ls) = putTab l >> putStr "\n\n" >> lineByLine ls







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
mcmcStep :: RandomGen a => Double -> (Tab, Tuning, Int, Double, a) -> (Tab, Tuning, Int, Double, a)
mcmcStep beta (tab1, tun, n1, score1, rand1) = 
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
        in (tab2, tun, n2, score2, rand3)


--An example temperature profile for simulated annealing. The inverse temperature increases
--linearly with step size in this example.
tempProfile :: Int -> Double
tempProfile n = (fromIntegral n)/100


--Performs simulated annealing infinitely many times, using a given temperature profile,
--and collects the results into an infinite list.
mcmcAll :: RandomGen a => Tab -> Tuning -> (Int -> Double) -> a -> [(Tab, Tuning, Int, Double, a)]
mcmcAll tab tun beta rand = 
	let score = scoreTab tab
	    loc = 0
	    first = (tab, tun, loc, score, rand)
            n = 0
            next num el = mcmcStep (beta num) el
            stepList num el = next num el : stepList (num+1) (next num el)
	in stepList 0 first


--Selects the optimal tab from a list of Simulated Annealing results.
mcmcBest :: Int -> [(Tab, Tuning, Int, Double, a)] -> Tab
mcmcBest n tabs = (\(a,_,_,_,_)->a) $ maximumBy (comparing (\(_,_,_,s,_)->s)) $ take n $ tabs



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
--Positions
--

--Functions to facilitate creating positions

--sort of Cartesian product: given lists of positions as and bs, it returns the set of all positions
--where the fingers press a combination of an a from as and a b from bs. 
cross :: [Position] -> [Position] -> [Position]
as `cross` bs = [ M.unionWith (error "Not Disjoint") a b | a <- as, b <- bs]
crossAll :: [[Position]] -> [Position]
crossAll = foldr cross []

--gives all of the translates of a position up and down the fretboard
copysOf :: Position -> [Position]
copysOf pos = let pressedFrets = M.map getFret pos
		  theMax = M.fold max 0 pressedFrets
 		  theMin = M.fold min 22 pressedFrets
	      in if M.null pressedFrets then [pos] else [ transPos n pos | n <- [1-theMin..22-theMax] ]

--gives all of the translates up and down the strings
upAndDown :: Position -> [Position]
upAndDown pos =
	let pressedStrings = filter ((>0).snd.maxFretFinger pos) (each::[GString])
            range = [0+fromEnum (minimum pressedStrings)..5-fromEnum (maximum pressedStrings)]
	in [ M.map (mapFinger (toEnum.(+r).fromEnum)) pos | r <- range]

--Here's where I begin enumerating some possible positions.

emptyPos = position []

singleFinger finger = [ position [(finger, On string (fromEnum finger))] | string <- each]
indexF = singleFinger F2
middleF = singleFinger F3
ringF = singleFinger F4
pinkyF = singleFinger F5

singleFingers = concat [indexF, middleF, ringF, pinkyF]

indexStdDigrams = indexF `cross` (concat [middleF, ringF, pinkyF])
f234StdTrigrams = indexF `cross` middleF `cross` ringF


indexBar = [position [(F2, Bar S1 S6 1)]]
barDigrams = indexBar `cross` (concat [middleF, ringF, pinkyF])
barTrigram = crossAll [indexBar, middleF, ringF]

f23SameString  = map (position.zip [F2,F3].map (\x-> On x 1))    $ choose 2 (each::[GString])
f32SameString  = map (position.zip [F3,F2].map (\x-> On x 1))    $ choose 2 (each::[GString])
f234SameString = map (position.zip [F2,F3,F4].map (\x-> On x 1)) $ choose 3 (each::[GString])
f432SameString = map (position.zip [F4,F3,F2].map (\x-> On x 1)) $ choose 3 (each::[GString])

sameString = concat [f23SameString,f32SameString,f234SameString,f432SameString]




csPos  = position [(F2,Bar S1 S6 1), (F3,On S5 2), (F4,On S3 3), (F5,On S2 4)]
asPos  = position [(F2, Bar S1 S6 1), (F4,Bar S3 S5 3)]
fPos   = position [(F2,Bar S1 S6 1), (F3,On S4 2), (F4,On S3 3), (F5,On S2 3)]
fmPos   = position [(F2,Bar S1 S6 1), (F4,On S3 3), (F5,On S2 3)]


csPos2 = upAndDown $ position [(F2,On S6 1), (F3, On S4 1), (F4, On S5 2)]
eMamPos  = upAndDown $ position [(F2,On S5 1),(F3, On S4 2),(F4, On S3 2)]



manualPos = concat [[csPos, asPos, fPos, fmPos], eMamPos, csPos2 ]


allPositionsUnsorted = emptyPos : (concatMap copysOf $
	 concat [singleFingers, indexStdDigrams, f234StdTrigrams, barDigrams, sameString, manualPos])
allPositions = sortBy (flip $ comparing scorePos) allPositionsUnsorted












--
--Tunings
--

stdTuning = tuning [(E,3), (A,3), (D,4), (G,4), (B,4), (E,5)]
dropDTuning = M.adjust (const (D,3)) S1 stdTuning
imyoursTuning = M.adjust (const (G,3)) S2 dropDTuning
openCTuning = tuning [(C,3), (G,3), (C,4), (G,4), (C,5), (E,5)]


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
mistTab = midiToTab "john_butler_trio_mist.mid" dropDTuning

mistTabOutput :: Tab -> IO ()
mistTabOutput = lineByLine . annotatedTab 3 0 (1/8) 6

stdMistTabOutput :: IO ()
stdMistTabOutput = mistTab >>= mistTabOutput

mistTabPositions :: IO ()
mistTabPositions = mistTab >>= tabPositions

mistMCMC :: Int -> IO Tab
mistMCMC n = do
	mt <- mistTab
	results <- stdmcmcAll mt dropDTuning
	print $ take n $ map (\(_,_,n,s,_) -> (n,s)) results
        return (mcmcBest n results)


stdmcmcAll :: Tab -> Tuning -> IO [(Tab, Tuning, Int, Double, StdGen)]
stdmcmcAll tab tuning = getStdGen >>= return . mcmcAll tab tuning tempProfile
