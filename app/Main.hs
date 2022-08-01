module Main where

import           Lib (alternate, digits, merge, odds, reverse, runningtotal)

main :: IO ()
main = do
  print (Lib.reverse [1 .. 100])
  print (odds [100 .. 199])
  print (runningtotal [1 .. 10])
  print (alternate [1 .. 20] [101 .. 110])
  print (merge [0,10 .. 100] [50 .. 150])
  print (digits (-1341514621456425))
