{-# LANGUAGE ViewPatterns #-}
--
-- code is from the user mstksg
-- didn't really know zippers till now
import Data.List

  
data Tape a = Tape { _tLeft :: [a]
                     , _tFocus :: a
                     , _tRight :: [a] } 
  deriving Show


move :: Int -> Tape Int -> Maybe (Tape Int)
move n (Tape lft x rs) = case compare n 0 of
  LT -> case lft of
    []      -> Nothing
    (l:ls') -> move (n + 1) (Tape ls' l  (x:rs))
  EQ -> Just (Tape lft x rs) 
  GT -> case rs of 
    []      -> Nothing
    (r:rs') -> move (n - 1) (Tape (x:lft) r rs')

step :: (Int -> Int) -> Tape Int -> Maybe (Tape Int)
step f (Tape ls x rs) = move x (Tape ls (f x) rs)


iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x0 = x0 : unfoldr (fmap dup . f) x0
  where
    dup x = (x,x)

day05a :: Tape Int -> Int
day05a = length . iterateMaybe (step update)
  where
    update x = x + 1

main :: IO ()
main = do
  xs <- parse <$> readFile "../data17/data05.txt"
  print $ day05a xs 

parse :: String -> Tape Int
parse (map read.lines->x:xs) = Tape [] x xs
parse _                      = error "Expected at least one line"
