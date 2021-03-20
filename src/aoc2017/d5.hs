
main :: IO ()
main = do
  xs <- lines <$> readFile "../data17/data05.txt"
  print $ map (\x -> read x :: Int) xs


