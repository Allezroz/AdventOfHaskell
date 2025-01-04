{- 2*l*w + 2*w*h + 2*h*l + smallest side-}

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseGift :: String -> [String]
parseGift s = case dropWhile (=='x') s of
                      "" -> []
                      s' -> w : parseGift s''
                            where (w, s'') = break (=='x') s'

giftWrap :: [String] -> Int
giftWrap [x,y,z] = (sum faces)*2 + (foldr1 min faces)
	where
		l = read x :: Int
		w = read y :: Int
		h = read z :: Int
		faces = [l*w,w*h,h*l]

{- i find the explicit recursion more readable idk -}
day21 = do
	contents <- readLines "day2.txt"
	let x = sum (map giftWrap (map parseGift contents))
	print x