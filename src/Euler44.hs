module Euler44 where

import Data.Set (fromList)
import Euler43
import Data.List.Split(chunksOf)
  
ans = concatMap find (chunksOf 10 (pentagonal 200000))  
find:: [Integer]->[(Integer,Integer)]  
find p = filter(\(x,y)->x>y && ((x + y) `elem` s) && ((x - y) `elem` s)) [(x,y)|x<-p, y<-p]  
s = fromList(pentagonal 40000)
pentagonal::Integer->[Integer]
pentagonal n = map (\k-> k*(3*k-1) `div` 2) [1..n]


