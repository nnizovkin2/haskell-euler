module Main where

import Euler42
      
main:: IO()
main = do s <- readFile "p042_words.txt"
          print(twn s)

