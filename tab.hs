module Tab where
import Euterpea
import qualified Data.Map as M
import Data.Maybe (catMaybes, isJust, fromMaybe, fromJust, listToMaybe)
import Data.List as List (find)
import Data.List (sortBy, nub, maximumBy, groupBy)
import Data.Ord (comparing)
import Control.Applicative ((<|>), liftA2)


--Data Type Declarations

class (Bounded a, Enum a) => Finite a where
  each :: [a]
  each = [minBound..maxBound]

data FingeringType a = Bar GString GString a | On GString a deriving (Show, Eq)
instance Functor FingeringType where
  fmap f (Bar s1 s2 x) = Bar s1 s2 (f x)
  fmap f (     On s x) =      On s (f x)

type FingeringP = FingeringType Fret
type Fingering = Maybe FingeringP

data Finger = F1 | F2 | F3 | F4 | F5 deriving (Bounded, Enum, Show, Eq, Ord)
instance Finite Finger where
  each = [minBound..maxBound]

data GString = S1 | S2 | S3 | S4 | S5 | S6 deriving (Bounded, Enum, Show, Eq, Ord)
instance Finite GString where
  each = [minBound..maxBound]

type Fret = Int

type Position = M.Map Finger Fingering

type Tuning = M.Map GString Pitch

type PosContext = (Position, M.Map GString PitchT)

type PitchT = (Pitch, PTime)



--General use functions

pp :: Show a => [a] -> IO()
pp = mapM_ print

pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

select :: Eq a => b -> [(a, b)] -> a -> b
select def xs x = maybe def snd $ List.find (\y -> (fst y)==x) xs

disjointOr :: Maybe a -> Maybe a -> Maybe a
disjointOr (Just _) (Just _) = error "disjointOr: Both values are just!"
disjointOr a b = a <|> b

choose :: Int -> [a] -> [[a]]
choose n xs | n < 0 = error "Must choose positive number."
choose 0 xs = [[]]
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
choose n [] = []

fromListAll xs = M.fromList (zip each xs)

fromListSome :: (Finite a, Ord a) => [(a, b)] -> M.Map a (Maybe b)
fromListSome xs = fromListAll $ [M.lookup x (M.fromList xs) | x <- each]

maybeF :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
maybeF f (Just x) (Just y) = Just (f x y)
maybeF _  Nothing (Just y) = Just y
maybeF _ (Just x) Nothing  = Just x
maybeF _  Nothing Nothing  = Nothing

scoreFuncs :: Num b =>  [a -> b] -> a -> b
scoreFuncs fs x = foldr (\f acc -> acc + (f x)) 0 fs

scoreFuncs2 :: Num c =>  [a -> b -> c] -> a -> b -> c
scoreFuncs2 fs x y = foldr (\f acc -> acc + (f x y)) 0 fs



splitU :: Ord k => k -> M.Map k a -> (M.Map k a, M.Map k a)
splitU k map = let (l,m,u) = M.splitLookup k map
                   newu = fromMaybe u $ fmap (\x-> M.insert k x u) m
                   in (l, newu)





--
--Guitar Tablature Functions
--

--Fingering Functions

getFret :: Fingering -> Maybe Fret
getFret (Just (On s f))      = Just f
getFret (Just (Bar s1 s2 f)) = Just f
getFret Nothing 	     = Nothing

fingerFret :: GString -> Fingering -> Fret
fingerFret x (Just (On s f))      = if x==s then f else 0
fingerFret x (Just (Bar s1 s2 f)) = if x `elem` [s1..s2] then f else 0
fingerFret x Nothing 	          = 0

isBar :: Fingering -> Bool
isBar (Just (Bar _ _ _)) = True
isBar _ = False

--Position Functions

position :: [(Finger, FingeringP)] -> M.Map Finger Fingering
position = fromListSome

frets :: Position -> M.Map GString Fret
frets p = M.fromList $ [(s, fst $ maxFretFinger p s) | s <- each::[GString] ] 

maxFretFinger :: Position -> GString -> (Fret, Maybe Finger)
maxFretFinger p s = let fretOf x = fingerFret s (p M.! x)
                        finger = listToMaybe $ sortBy (comparing fretOf) [ x | x <- each::[Finger], fretOf x > 0 ]
                    in (fromMaybe 0 ((fmap fretOf) finger) , finger)


requiredPos :: M.Map GString PitchT -> Position -> Position
requiredPos strs pos = let fingers = catMaybes $ map (snd.(maxFretFinger pos)) $ M.keys strs
                       in M.filterWithKey (\k _ -> k `elem` fingers) pos

transPos :: Int -> Position -> Position
transPos x = M.map $ (fmap.fmap) (+x)


pitches :: Tuning -> Position -> M.Map GString Pitch
pitches t pos = M.intersectionWith trans (frets pos) t


--Tuning functions

tuning :: [Pitch] -> M.Map GString Pitch
tuning = fromListAll

--Functions for playing notes with positions

playNoteWith :: M.Map GString Pitch -> PitchT -> Maybe GString
playNoteWith possPitches p = let playPitches = (M.keys . M.filter (fst p==)) possPitches
			     in if ([]==playPitches) then Nothing else Just (maximum playPitches)

playNotesWith :: M.Map GString Pitch -> [PitchT] -> Maybe (M.Map GString PitchT)
playNotesWith possPitches (p:ps) = do x <- playNoteWith possPitches p; 
					fmap (M.insert x p) $ playNotesWith possPitches ps
playNotesWith possPitches [] = Just M.empty



suitablePositions :: Tuning -> PosContext -> [PitchT] -> [(PosContext, Double)]
suitablePositions t pc ps = let toPlay p = playNotesWith (M.difference (pitches t p) (snd pc)) ps
                            in catMaybes $ [do newMap <- toPlay pos; Just ((pos,newMap), score) | (pos,score) <- (restrictedPositions pc)]

restrictedPositions :: PosContext -> [(Position, Double)]
restrictedPositions (pos,_) = [ p | p <- allPositions, M.null $ M.filter (id) $ M.intersectionWith (/=) (fst p) pos ]

restrictedStrings :: PosContext -> [GString]
restrictedStrings (_, pitchMap) = M.keys pitchMap


--
--Scoring Positions
--

--Calculated properties about positions

fingerNaturalPos :: Fractional a => Finger -> a
fingerNaturalPos = select (error "Not a finger?") [(F1,1),(F2,0),(F3,1),(F4,3/2),(F5,2)]

handPosition :: Position -> Maybe Fret
handPosition pos = M.fold (maybeF max) Nothing $ M.map getFret pos

numFingers :: Position -> Int
numFingers pos = M.size $ M.filter isJust pos

numBars :: Position -> Int
numBars pos = M.size $ M.filter isBar pos


--Individual scoring functions

scorePosFingers :: Fractional a => Position -> a
scorePosFingers pos = (-1) * select (error "Too many fingers!") [(0, 0), (1, 1), (2, 5), (3, 10), (4, 30), (5, 200)] (numFingers pos)

scorePosHandPosition :: Fractional a => Position -> a
scorePosHandPosition pos = ((-1)/5)*fromIntegral (fromMaybe 0 $ handPosition pos)

scorePosNumBars :: Fractional a => Position -> a
scorePosNumBars pos = (-1) * select (200) [(0,0), (1, 15), (2, 50)] (numBars pos)


--Aggregate scoring function

scorePos :: Position -> Double
scorePos = scoreFuncs funcs

funcs = [scorePosFingers,scorePosHandPosition,scorePosNumBars]

--
--Tunings
--

stdTuning = tuning [(E,3), (A,3), (D,4), (G,4), (B,4), (E,5)]
dropDTuning = M.adjust (const (D,3)) S1 stdTuning
openCTuning = tuning [(C,3), (G,3), (C,4), (G,4), (C,5), (E,5)]






--
--Differences in Positions
--

isSlideFinger :: Fingering -> Fingering -> Bool
isSlideFinger _ Nothing = True
isSlideFinger (Just (Bar s1 s2 _)) (Just (Bar t1 t2 _)) = s1==t1 && s2==t2
isSlideFinger (Just (On s _)) (Just (On t _)) = s==t
isSlideFinger _ _ = False

isSlide :: Position -> Position -> Bool
isSlide pos1 pos2 = M.fold (&&) True $ M.intersectionWith isSlideFinger pos1 pos2

slideVar :: Position -> Position -> Int
slideVar pos1 pos2 = let vars = M.intersectionWith (liftA2 (-)) (M.map getFret pos1) (M.map getFret pos2)
		     in fromJust $ (liftA2 (-)) (M.fold (maybeF max) Nothing vars) (M.fold (maybeF min) Nothing vars)

addProp :: (Fingering -> Bool) -> Position -> Position -> Int
addProp f pos1 pos2 =  M.size $ M.filter id $ M.intersectionWith (\x y -> x && f y) (M.intersectionWith (/=) pos1 pos2) pos2

addFingers :: Position -> Position -> Int
addFingers = addProp isJust

removeFingers :: Position -> Position -> Int
removeFingers = flip addFingers

addBars :: Position -> Position -> Int
addBars = addProp isBar

removeBars :: Position -> Position -> Int
removeBars = flip addBars



addFingersScore p1 p2 = (-1)*(select (error "Fingers") [(0, 0), (1, 1), (2, 5), (3, 10), (4, 30), (5, 200)] $ addFingers p1 p2)

removeFingersScore p1 p2 = (-1/4)*(select (error "Fingers") [(0, 0), (1, 1), (2, 5), (3, 10), (4, 30), (5, 200)] $ removeFingers p1 p2)

addBarsScore p1 p2 = (-1)*(select 1000 [(0, 0), (1, 20), (2, 50)] $ addBars p1 p2)


removeBarsScore p1 p2 = (-1)*(select 50 [(0, 0), (1, 5), (2, 15)] $ removeBars p1 p2)

changeHandPositionScore :: Position -> Position -> Double
changeHandPositionScore p1 p2 =(-1)*(fromIntegral $ fromMaybe 0 $ fmap abs $ liftA2 (-) (handPosition p1) (handPosition p2))

changePosScore :: Position -> Position -> Double
changePosScore = scoreFuncs2 changeFuncs

changeFuncs = [addFingersScore, removeFingersScore, addBarsScore, removeBarsScore, changeHandPositionScore]




--
--Positions
--

--Functions to facilitate creating positions

cross :: [Position] -> [Position] -> [Position]
as `cross` bs = [ M.unionWith disjointOr a b | a <- as, b <- bs]
crossAll :: [[Position]] -> [Position]
crossAll = foldr cross []


copysOf :: Position -> [Position]
copysOf pos = let pressedFrets = M.map fromJust $ M.filter isJust $ M.map getFret pos
		  theMax = M.fold max 0 pressedFrets
 		  theMin = M.fold min 22 pressedFrets
	      in if M.null pressedFrets then [pos] else [ transPos n pos | n <- [1-theMin..22-theMax] ]

--Positions

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
csPos2 = position [(F2,On S6 1), (F3, On S4 1), (F4, On S5 2)]

manualPos = [csPos, asPos, csPos2]


allPositionsUnsorted = emptyPos : (concatMap copysOf $
	 concat [singleFingers, indexStdDigrams, f234StdTrigrams, barDigrams, sameString, manualPos])
allPositions = sortBy (\x y -> compare (snd y) (snd x)) [ (x, scorePos x) | x <- allPositionsUnsorted]


--
--Chords
--

dChord,cChord :: [Pitch]
dChord = [(A,4), (D, 5), (Fs,5)]

cChord = [(C,4),(E,4),(G,4),(C,5),(E,5)]




















--
--Performance to Note sets
--

defPerform1 :: Music1 -> Performance
defPerform1 = perform defPMap defCon

defPerform :: Music Pitch -> Performance
defPerform = defPerform1.toMusic1

playingAtTime :: PTime -> Performance -> Performance
playingAtTime t events = filter ( \e -> (t >= (eTime e) && t < (eTime e + eDur e) ) ) events

toNoteSet :: Performance -> [(PTime, [PitchT])]
toNoteSet events = map (\l -> (fst $ head l, map snd l)) $ groupBy (\x y -> fst x==fst y) [(eTime e, (pitch $ ePitch e, eTime e + eDur e)) | e <- events]

pairWise :: Ord k => [(k, a)] -> M.Map (k, k) (a, a)
pairWise list = M.fromList $ map (\ ((a,b),(c,d)) -> ((a,c),(b,d)) ) $ pairs list









--
--Example Music
--

ttls = let 	l1 = ([c, c, g, g, a, a], g)
		l2 = ([f, f, e, e, d, d], c)
		l3 = ([g, g, f, f, e, e], d)
		mkLine (ls, n) = line (map (\f -> f 4 qn) ls) :+: n 4 hn
       in line $ map mkLine [l1, l2, l3, l3, l1, l2]




--
--
--

emptyPC :: PosContext
emptyPC = (M.empty, M.empty)

selfishTab :: Tuning -> [(PTime, [PitchT])] -> [(PTime, (PosContext, Double) )] 
selfishTab t noteset = map (\(time,ps) -> (time, selfishPos t ps)) noteset

selfishPos :: Tuning -> [PitchT] -> (PosContext,Double)
selfishPos t ps = let suitable = suitablePositions t emptyPC ps
                     in if suitable==[]
                     then error $ "No suitable positions for " ++ (show ps)
                     else head suitable


updatePosContext :: PTime -> PosContext -> PosContext
updatePosContext t (pos, pitchMap) = let newPitches = M.filter ((>t).snd) pitchMap
                                     in (requiredPos newPitches pos , newPitches)


forwardTabNext :: Tuning -> (PTime, (PosContext, Double)) -> (PTime, [PitchT]) -> (PTime, (PosContext, Double))
forwardTabNext tun (_, (pc, _)) (t, ps) = (t, maximumBy (comparing f) $ suitablePositions tun (updatePosContext t pc) ps)
        where f ((p,_), score) = score + changeHandPositionScore (fst pc) p


forwardTab :: Tuning -> [(PTime, [PitchT])] -> [(PTime, (PosContext, Double) )] 
forwardTab t noteset = extend (pt1, (head $ suitablePositions t emptyPC pitches1)) (tail noteset) where
	extend first (p:ps) = first : extend (forwardTabNext t first p) ps
	extend first [] = [first]
	(pt1, pitches1) = head noteset





scoreTab :: M.Map PTime (Position, Double, [GString]) -> Double
scoreTab tab = let singleScore = M.fold (+) 0 $ M.map (\(a,b,c) -> b) tab
		   posDiff = pairWise $ M.toList $ M.map (\(a,b,c) -> a) tab
		   diffScore = M.fold (+) 0 $ M.map (\(a,b) -> changePosScore a b) posDiff
	       in singleScore + diffScore


asciiTab :: PTime -> PTime -> PTime -> M.Map PTime (PosContext, Double) -> [String]
asciiTab start acc meas tab = let t = start + acc
                                  (lower,upper) = splitU t tab
                                  firstToUse = map snd $ M.toList lower
                                  nextThing = if (M.null upper) then take 6 $ repeat [] else asciiTab t acc meas upper
                              in zipWith (++) (tabFor firstToUse) nextThing

putTab :: [String] -> IO ()
putTab strs = putStr $ unlines $ reverse strs

tabFor :: [(PosContext, Double)] -> [String]
tabFor pcs = let try str ((po,pi),_) = if (M.member str pi) then Just (fst (maxFretFinger po str)) else Nothing 
             in [ twoDnum $ listToMaybe $ catMaybes $ map (try s) pcs  | s<-each::[GString]]


twoDnum :: Maybe Fret -> String
twoDnum (Just n) | n < 10    = '-' : show n
                 | otherwise = show n
twoDnum Nothing              = "--"

addCharN :: Char -> Int -> String -> String
addCharN c n [] = [c]
addCharN c n str = c : (take n str) ++ addCharN c n (drop n str)

addInfo :: PTime -> PTime -> [String] -> [String]
addInfo acc meas ls = let n = round (meas/acc) in 
                   [addCharN '|' n l | l <- ls]

putsTab :: Int -> [String] -> IO ()
putsTab n strs | head strs==[] = return ()
               | otherwise = putTab (map (take n) strs) >> putStr "\n\n" >> putsTab n (map (drop n) strs) 




----From Hudak's EuterpeaExamples.lhs

loadMidiFile fn = do
  r <- importFile fn 
  case r of
    Left err -> error err
    Right m  -> return m

tcd file = do
             x <- loadMidiFile file
             return $ (fst3.fromMidi) x

fst3 (a,b,c) = a
