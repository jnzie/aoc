import Data.List
import Data.Char

conseqs :: [a] -> [(a,a)]
conseqs []     = []
conseqs (x:xs) = zip (x:xs) (xs ++ [x])

f l = filter (\x -> length x > 1) $ group l 

main :: IO ()
main = do 
        xs <- readFile "../data17/data01.txt" 
        print $ sum $ map (digitToInt . fst) $ filter (uncurry (==)) $ conseqs xs

ff :: [Int] -> Int -> Int
ff []     acc = acc
ff lst@(x:xs) acc = if x == (lst !! (pos - 1)) then ff xs (acc+x) else ff xs acc
              where pos = length lst `div` 2

pt2 :: IO ()
pt2 = do
        xs <- readFile "../data17/data01.txt" 
        print $ ff (map digitToInt (filter (\x -> x `elem` ['0'..'9']) xs)) 0
        
