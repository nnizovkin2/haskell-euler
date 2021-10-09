module Main where

import Euler2(fibSeq)
import Euler25(fibIndex)

main :: IO ()

main = do
   print(fibIndex (10^999))
