import qualified Data.List.Split as T
import qualified Data.List as Lst
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Control.Monad (forM_)
import Control.Monad (foldM)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

parseInstruction :: [String] -> (String,Int,Int)
parseInstruction s
    | s !! 0 == "turn" = (s !! 1,
                         read (head (T.splitOn "," (s !! 2))) * 1000 + read (last (T.splitOn "," (s !! 2))),
                         read (head (T.splitOn "," (s !! 4))) * 1000 + read (last (T.splitOn "," (s !! 4))))
    | otherwise = (s !! 0,
                  read (head (T.splitOn "," (s !! 1))) * 1000 + read (last (T.splitOn "," (s !! 1))),
                  read (head (T.splitOn "," (s !! 3))) * 1000 + read (last (T.splitOn "," (s !! 3))))

 -- not ACTUALLY for start..end
 -- really needs to be a nested loop

 -- [1..1000 of [1..1000 of 1/0]] allows for the same slicing

 -- work out the vectors and monads for myself

switchLightsMutable :: V.Vector Int -> (String,Int,Int) -> IO (V.Vector Int)
switchLightsMutable vec (instruction,start,end) = do
    mv <- V.thaw vec
    forM_ [start..end] $ \i -> do
        case instruction of
            "on" -> MV.write mv i 1
            "off" -> MV.write mv i 0
            "toggle" -> do
                val <- MV.read mv i
                MV.write mv i (1 - val)
    V.freeze mv

day61 = do
    inp <- readLines "day6.txt"
    let initialLights = V.replicate 1000000 0
    let instructions = map (parseInstruction . words) inp
    finalLights <- foldM switchLightsMutable initialLights instructions
    print $ V.sum finalLights
    print finalLights