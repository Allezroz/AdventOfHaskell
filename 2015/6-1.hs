import qualified Data.List.Split as T
import qualified Data.List as Lst

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseInstruction :: [String] -> (String, Int, Int, Int, Int)
parseInstruction s
    | s !! 0 == "turn" = (s !! 1,
                         read (head (T.splitOn "," (s !! 2))), read (last (T.splitOn "," (s !! 2))),
                         read (head (T.splitOn "," (s !! 4))), read (last (T.splitOn "," (s !! 4))))
    | otherwise = (s !! 0,
                  read (head (T.splitOn "," (s !! 1))), read (last (T.splitOn "," (s !! 1))),
                  read (head (T.splitOn "," (s !! 3))), read (last (T.splitOn "," (s !! 3))))

switchLightsOuter :: [[Int]] -> (String, Int, Int, Int, Int) -> [[Int]]
switchLightsOuter [] _ = []
switchLightsOuter l (instruction, xs, ys, xe, ye) =
    let pre = take xs l
        post = drop (xe + 1) l
        ls = take (xe - xs + 1) (drop xs l)
        modified = map (\row -> switchLightsInner row instruction ys ye) ls
    in pre ++ modified ++ post

switchLightsInner :: [Int] -> String -> Int -> Int -> [Int]
switchLightsInner l instruction ys ye =
    let pre = take ys l
        post = drop (ye + 1) l
        ls = take (ye - ys + 1) (drop ys l)
        modified = case instruction of
            "on"  -> replicate (length ls) 1
            "off" -> replicate (length ls) 0
            _     -> map (1-) ls  -- toggle case
    in pre ++ modified ++ post

day61 :: IO ()
day61 = do
    inp <- readLines "day6.txt"
    let initialLights = replicate 1000 (replicate 1000 0)
    let instructions = map (parseInstruction . words) inp
    let finalLights = foldl switchLightsOuter initialLights instructions
    
    print $ sum $ map sum finalLights