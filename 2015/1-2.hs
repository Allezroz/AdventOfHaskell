{- open para = +1 close para = -1, what index causes him to enter the basement?-}

day1p2 :: [Char] -> Int
day1p2 xs = head $ 
    [x | (x, sum) <- zip [1..] (scanl (+) 0 (map bracketToNum xs)), sum < 0]
  where
    bracketToNum '(' = 1
    bracketToNum ')' = -1

{- i find the explicit recursion more readable idk -}

main = do
	contents <- readFile "day1.txt"
	let tst = day1p2 contents
	print tst

