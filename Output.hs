module Output where
import Positions
import Tablature (Tab, PosContext, tPos)

import Euterpea (PTime)
import qualified Data.Map as M
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)



--Splits a map, and inserts the middle value (if it exists)
--into the upper map.
splitU :: Ord k => k -> M.Map k a -> (M.Map k a, M.Map k a)
splitU k map = let (l,m,u) = M.splitLookup k map
                   newu = fromMaybe u $ fmap (\x-> M.insert k x u) m
                   in (l, newu)


--
tabStart :: Tab -> PTime
tabStart = minimum . map ptStart . M.elems . snd . snd . M.findMin

tabLargerThan :: PTime -> Tab -> Bool
tabLargerThan t tab = (not . M.null . snd . M.split t) tab ||
	(maximum . map ptEnd . M.elems . snd . snd . M.findMax) tab >= t



--
--Tablature output functions
--


--Given the "bin width" (acc) of time for which each vertical line in the tab should
--represent (the "resolution" of the tab) and the number
asciiTab :: PTime -> PTime -> Tab -> [String]
asciiTab acc meas tab = 
	let start = tabStart tab
            list t = if (tabLargerThan t tab) then t:(list (t+acc)) else []
            times = list start
	    pairedtimes = zip times (tail times)
        in foldr1 (zipWith (++)) $ map (asciiTabPiece tab) pairedtimes

--Given a start and end time, gives one line with all the notes played at that time
asciiTabPiece :: Tab -> (PTime, PTime) -> [String]
asciiTabPiece tab (start,end) = 
	let (lower,_) = splitU end tab
	    (_,middle) = splitU start lower
	in tabFor start end (M.elems middle)



--Reverses the list of strings to put highest pitch strings on top, and outputs them.
putTab :: [String] -> IO ()
putTab = putStr.unlines.reverse


--Gives a single vertical bin/bar of tab, given the start of that window (start) and the end,
--and the PosContext to be played during that time.
tabFor :: PTime -> PTime -> [PosContext] -> [String]
tabFor start end pcs = let try str (po,pi) = if (M.member str $ M.filter timefilter pi) then Just (snd (maxFretFinger po str)) else Nothing 
                           timefilter pitcht = ptStart pitcht>=start && ptStart pitcht<end
                           unannotated s = (twoDnum.listToMaybe.catMaybes.map (try s)) pcs
               in map unannotated (each::[GString])



--Formats tab, and splits it into lines, and adds measure bars.
annotatedTab :: Int-> PTime -> PTime -> Tab -> [[String]]
annotatedTab n acc meas tab = 
	map (map (++"|")) $ chunks ((round(meas/acc)+1)*n) $ addInfo acc meas $ asciiTab acc meas tab


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
                   [addStr "|" n 0 l ++ "|" | l <- ls]


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

