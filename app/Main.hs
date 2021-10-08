module Main where

import Euler13

main :: IO ()

readInt:: String->Integer
readInt s = read s::Integer 
main = do
  content <- readFile "data.txt"
  print(sumOfInts(lines content))  
