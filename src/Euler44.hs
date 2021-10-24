module Euler44 where

import Data.Set (fromList)
import Data.List.Split(chunksOf)
import Euler43

--ref
--https://www.xarg.org/puzzle/project-euler/problem-44/
--Count in java. Only code  
ans = a (pentagonal 5000)
a:: [Integer]->Integer 
a l = minimum (map(uncurry (-)) (filter (\(x, y)->((x-y) `elem` s) && ((x + y) `elem` s)) [(x, y)| x<-l, y<-l]))
  
s = fromList(pentagonal 10000)

pentagonal::Integer->[Integer]
pentagonal n = map (\k-> k*(3*k-1) `div` 2) [1..n]


