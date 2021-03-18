import Data.List.Split

f :: [[Int]] -> Int
f xs = foldr (\x y -> (abs  ((head x) - (last x))) + y) 0 xs

main :: IO ()
main = do
  xs <-  readFile "../data17/data02.txt" 
  print $ f $ map (map (\x -> read x :: Int)) $ map (splitOn "\t") $ lines xs
