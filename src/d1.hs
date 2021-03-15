import Data.List
import Data.Char
conseqs :: [a] -> [(a,a)]
conseqs []     = []
conseqs (x:xs) = zip (x:xs) (xs ++ [x])

f l = filter (\x -> length x > 1) $ group l 

main :: IO ()
main = do 
        xs <- readFile "./data/data01.txt" 
        print $ sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ conseqs xs

