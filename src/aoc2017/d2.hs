import Data.List.Split

f :: [[Int]] -> Int
f xs = foldr (\x y -> (abs  ((minimum x) - (maximum x))) + y) 0 xs

main :: IO ()
main = do
  xs <-  readFile "../data17/data02.txt" 
  print $ res $ map (map (\x -> read x :: Int)) $ map (splitOn "\t") $ lines xs



main2 :: IO ()
main2 = do
  xs <-  readFile "../data17/data02.txt" 
  print $ f $ parse xs 
  print $ res $ parse xs 

parse :: String -> [[Int]]
parse = map (map read . words) . lines


res :: [[Int]] -> Int
res  = sum . map checkA 

checkA :: [Int] -> Int
checkA xs = maximum xs - minimum xs 
