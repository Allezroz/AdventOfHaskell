{- ^v<> walk in that direction. Drops a gift at starting point plus after each step. How many houses get a gift?-}
{- day 2 santa takes the first/third/etc element, robosanta takes 2/4/  etc -}

import qualified Data.Hash.MD5 as MD5

-- Convert string to MD5 hex string
md5String :: String -> String
md5String input = MD5.md5s $ MD5.Str input

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

findMD5 :: String -> String
findMD5 x = head $ [y | y <- [md5String (x ++ show n) | n <- [0..]], take 5 y == "00000"]

findMD5num :: String -> Int
findMD5num x = head $ [n | n <- [0..], take 6 (md5String (x ++ show n)) == "000000"]


day42 = do
	let inp = "ckczppom"
	let tst = findMD5num inp
	print (tst)
