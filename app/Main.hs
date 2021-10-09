module Main where

import Euler22(totalScore)

main :: IO ()

main = do s <- readFile "p022_names.txt" 
          print(totalScore s)
