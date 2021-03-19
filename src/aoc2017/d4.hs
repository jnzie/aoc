import Data.List

main :: IO ()
main = do
  xs <- map words . lines <$> readFile "../data17/data04.txt" 
  print $ length $ filter (\x -> x == (nub x)) xs


