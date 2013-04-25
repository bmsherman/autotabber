module PositionBank where
import Positions
import qualified Data.Map as M


--
--Positions
--

--Functions to facilitate creating positions

--Returns a list of all sublists (with order maintained) of that list
--that are of length n.
choose :: Int -> [a] -> [[a]]
choose n xs | n < 0 = error "Must choose positive number."
choose 0 xs = [[]]
choose n (x:xs) = map (x:) (choose (n-1) xs) ++ choose n xs
choose n [] = []

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



