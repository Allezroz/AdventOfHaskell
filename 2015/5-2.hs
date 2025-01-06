{- It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements. -}
import qualified Data.Text as T

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

{- It contains a pair of any two letters that appears at least twice in the string without overlapping, like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
It contains at least one letter which repeats with exactly one letter between them, like xyx, abcdefeghi (efe), or even aaa.-}


-- I get that this is a pretty time inefficient way of doing this. I'll see if I have the enthusiasm to rewrite.
isNice :: String -> Int
isNice [] = 0
isNice s 
	| hasRepeat s == False = 0
	| hasPair s s == False = 0
	| otherwise = 1

hasPair :: String -> String -> Bool
hasPair _ [] = False
hasPair s (p:px)
	| length px < 1 = False	
	| T.count (T.pack "$") (T.replace (T.pack pat) (T.pack "$") (T.pack s)) > 1 = True
	| otherwise = hasPair s px 
	where pat = [p, head px]

getPair :: String -> String
getPair [] = []
getPair (s:sx)
	| (length sx) < 1 = []
	| otherwise = [s, head sx]


hasRepeat :: String -> Bool
-- repeats with one letter in between
hasRepeat [] = False
hasRepeat (s:sx)
	| length sx < 2 = False
	| s == sx !! 1 = True
	| otherwise = hasRepeat sx



day52 = do
	inp <- readLines "day5.txt"
	let tmp = sum(map isNice inp)
	print tmp
