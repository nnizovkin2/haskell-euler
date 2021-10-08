module Main where

import Euler14(maxLengthEl)

main :: IO ()

readInt:: String->Integer
readInt s = read s::Integer 
main = do
  print(maxLengthEl 1000000)  
