module Main where

-- Save this file as "Day01.hs"
-- Save your input as "Day01.txt"

solve1 :: String -> Int
solve1 s = 0 -- TODO implement me

-- Run me with $ stack runhaskell Day01.hs
main :: IO ()
main = do
  s <- readFile "Day01.txt"
  print (solve1 s)
