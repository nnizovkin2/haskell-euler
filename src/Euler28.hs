module Euler28 where
import Data.List.Split.Internals(chunksOf)  

res = 1 + 1000*2 + sum (zipWith (*) (map sum (chunksOf 4(map (*2) [1..2000]))) (reverse [1..500]))
