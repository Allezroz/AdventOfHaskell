{- It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
It contains at least one letter that appears twice in a row, like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
It does not contain the strings ab, cd, pq, or xy, even if they are part of one of the other requirements. -}
import qualified Data.Text as T

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile


-- I get that this is a pretty time inefficient way of doing this. I'll see if I have the enthusiasm to rewrite.
isNice :: String -> Int
isNice [] = 0
isNice s 
	| hasRepeat s == False = 0
	| isNaughty s == True = 0
	| threeVowels s == True = 1
	| otherwise = 0

hasRepeat :: String -> Bool
hasRepeat (s:sx)
	| sx == [] = False
	| s == head sx = True
	| otherwise = hasRepeat sx

isNaughty :: String -> Bool
isNaughty s = any (\needle -> T.isInfixOf (T.pack needle) (T.pack s)) naughtyList
	where naughtyList = ["ab","cd","pq","xy"]

threeVowels :: String -> Bool
threeVowels [] = False
threeVowels s = length (onlyVowels s) >= 3

onlyVowels :: String -> String
onlyVowels = filter (`elem` "aeiou")

day51 = do
	inp <- readLines "day5.txt"
	let tmp = sum(map isNice inp)
	print tmp
