module Euler50 where
  
import Euler10
import Data.Set (fromList)

ans = maximum(map(\(i,j)->(i-j, (sums!!i) - (sums!!j)))
      (filter (\(i,j)->((sums!!i) - (sums!!j)) `elem` set)
      [(x,y)| x<-[0..(length sums-1)], y<-[0..(length sums-1)]]))


sums = foldl (\l n -> l ++ [last l + n]) [0] (findPrimes 10000)
set = fromList(findPrimes 1000000)

