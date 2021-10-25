module Main where

import Euler54(res)
      
main:: IO()
main = do 
         content <- readFile "p054_poker.txt"
         print (res content)

-- content <- readFile (args !! 0)
-- linesOfFiles <- lines content