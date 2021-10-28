module Main where

import Euler59(decrypt, dec, text)
import Data.Bits(xor, (.&.))
import Data.Char (chr, ord, toUpper)
      
main:: IO()
main = do
         cipher<-readFile "p059_cipher.txt"
         corpus<-readFile "words.txt"
         print(decrypt cipher corpus)

-- content <- readFile (args !! 0)
-- linesOfFiles <- lines content