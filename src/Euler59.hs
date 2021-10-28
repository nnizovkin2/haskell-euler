module Euler59 where

import Data.List.Split (splitOn, chunksOf)
import Data.Char (chr, ord, toUpper, toLower)
import Data.Bits (xor)
import Data.Text.Internal.Search
import Data.Text.Conversions (convertText)

--prototype. result get in java
decrypt encrypted corpus = sum(map ord (head((\words->filter(\candidate->length(filter(\word->not 
                            (null (indices (convertText word) (convertText candidate))))
                           words)>150)
                           ([dec enc key|enc<-[text encrypted], key<-keys]))(filter(\w->length w > 2)(lines corpus)))))
dec::[Char]->[Char]->[Char]
dec t key = concatMap(\c->zipWith(\ch1 ch2->chr(ord ch1 `xor` ord ch2)) c key)(chunksOf 3 t) 
text::[Char]->[Char]
text rt = map(\s->chr(read s::Int))(splitOn "," rt)  
keys:: [[Char]]
keys=(\x y z->map (\ch -> chr(ch + ord 'a'))[x,y,z])<$>[0..25]<*>[0..25]<*>[0..25]