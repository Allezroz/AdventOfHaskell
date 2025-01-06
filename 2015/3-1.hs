{- ^v<> walk in that direction. Drops a gift at starting point plus after each step. How many houses get a gift?-}
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

day31 = do
	contents <- readFile "day3.txt"
	let tmp = Data.List.nub (deliver contents 0 0)
	print (length tmp)