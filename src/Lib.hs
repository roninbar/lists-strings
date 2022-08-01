{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Lib
  ( reverse
  , odds
  , runningtotal
  , alternate
  , merge
  , fib
  , digits
  , search
  , primes
  ) where

import           Prelude hiding (reverse)

reverse :: [a] -> [a]
reverse = reverse' []
  where
    reverse' as []     = as
    reverse' as (b:bs) = reverse' (b : as) bs

odds :: [a] -> [a]
-- odds xs = [xs !! k | k <- [1,3 .. length xs - 1]]
odds xs = (!!) xs <$> [1,3 .. length xs - 1] --   do k <- [1,3 .. length xs - 1]
    --      return (xs !! k)

-- odds xs = (>>=) [1,3 .. length xs - 1] (\k -> return (xs !! k))
runningtotal :: Num a => [a] -> [a]
runningtotal []     = []
runningtotal (x:xs) = x : map (x +) (runningtotal xs)

alternate :: [a] -> [a] -> [a]
alternate xs []         = xs
alternate [] ys         = ys
alternate (x:xs) (y:ys) = x : y : alternate xs ys

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) =
  if x < y
    then x : merge xs (y : ys)
    else y : merge (x : xs) ys

fib :: [Integer]
fib = 0 : 1 : zipWith (+) fib (tail fib)

digits :: Integer -> [Int]
digits = digits' []
  where
    digits' :: [Int] -> Integer -> [Int]
    digits' ds 0 = ds
    digits' ds n
      | n > 0 = digits' ((fromInteger (n `mod` 10) :: Int) : ds) (n `div` 10)

search :: Ord a => a -> [a] -> Maybe Int
search _ [] = Nothing
search x [y]
  | x == y = Just 0
  | otherwise = Nothing
search x xs
  | length xs > 1 =
    let k = length xs `div` 2
     in if x < xs !! k
          then search x (take k xs)
          else (k +) <$> search x (drop k xs)

primes :: [Int]
primes = sieve [2 ..]
  where
    sieve :: [Int] -> [Int]
    sieve (p:ns) = p : sieve (filter ((0 /=) . (`mod` p)) ns)
