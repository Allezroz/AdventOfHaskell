{- ^v<> walk in that direction. Drops a gift at starting point plus after each step. How many houses get a gift?-}
{- day 2 santa takes the first/third/etc element, robosanta takes 2/4/  etc -}

import Data.List
readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

deliver :: String -> Int -> Int -> [(Int,Int)]
deliver [] x y = [(x,y)]
deliver (s:sx) x y
  | s == '^' = [(x,y)] ++ (deliver sx x (y+1))
  | s == 'v' = [(x,y)] ++ (deliver sx x (y-1))
  | s == '<' = [(x,y)] ++ (deliver sx (x-1) y)
  | s == '>' = [(x,y)] ++ (deliver sx (x+1) y)


definePath :: String -> String
definePath [] = []
definePath (x:xs) = x : (definePath (drop 1 xs))

day32 = do
	contents <- readFile "day3.txt"
	let santa = Data.List.nub (deliver (definePath contents) 0 0)
	let robosanta = Data.List.nub (deliver (definePath (drop 1 contents)) 0 0)
	let tmp = Data.List.nub (santa ++ robosanta)
	print (length tmp)