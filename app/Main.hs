module Main where

import Euler17(lettersSize)
import Data.Char(isSpace)

main :: IO ()

readInt:: String->Integer
readInt s = read s::Integer
main = do
  print lettersSize
