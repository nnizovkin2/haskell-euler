module Euler45 where

import Data.Set (fromList)

--Count in java. Only code  
ans = filter (\n->(n `elem` triangle) && (n `elem` pentagonal)) hexagonal  
triangle = fromList(map (\n -> n*(n+1) `div` 2)[286..500000])
pentagonal = fromList(map (\n -> n*(3*n-1) `div` 2)[166..500000])
hexagonal = map (\n -> n*(2*n-1))[144..200000]
