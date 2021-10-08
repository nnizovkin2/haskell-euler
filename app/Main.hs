module Main where

import Euler16(ans)

main :: IO ()

readInt:: String->Integer
readInt s = read s::Integer 
main = do
  print ans
