module Main where

import Lib

main :: IO ()

--If we list all the natural numbers below 10 that are multiples of 3 or 5,
-- we get 3, 5, 6 and 9. The sum of these multiples is 23.
--
--Find the sum of all the multiples of 3 or 5 below 1000.

main = do
  print (countMult 1000)
  
--  putStrLn "What's your name?"
--  name <- getLine
--  putStrLn ("Hello " ++ name)