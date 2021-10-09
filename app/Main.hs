module Main where

import Euler24(toFactorialNotation, fromList)

main :: IO ()

main = do
   print (fromList(toFactorialNotation 999999))
