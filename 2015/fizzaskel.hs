fb :: Int -> String
fb x
	| mod x 15 == 0 = "FizzBuzz"
	| mod x 5 == 0 = "Buzz"
	| mod x 3 == 0 = "Fizz"
	| otherwise = show x


main :: IO()
main = do
	putStrLn "Enter a positive integer above 0"
	x <- getLine
	putStrLn "Output: "
	let out = (map fb [1..(read x)])
	mapM_ putStrLn out

