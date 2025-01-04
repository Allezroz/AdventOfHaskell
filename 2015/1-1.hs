{- open para = +1 close para = -1-}

day1 :: [Char] -> Int
day1 [] = 0
day1 (x:xs)
	| x == '(' = 1 + (day1 xs)
	| x == ')' = -1 + (day1 xs)

day1f :: [Char] -> Int
day1f = foldl (\acc x -> acc + if x == '(' then 1 else -1) 0

{- i find the explicit recursion more readable idk -}
main = do
	contents <- readFile "day1.txt"
	let tst = day1f contents
	print tst

