module Euler49 where

import Euler10
import Data.List (sort, nub)
import Data.Set (fromList)

ans = head(map(\(a1, a2, a3)->read(show a1 ++ show a2 ++ show a3)::Integer) 
      (filter(\(a1, a2, a3)->sort(show a1)==sort(show a2) && sort(show a2)==sort(show a3) && a1 /= 1487)
      (map (\n->(n, n+3330, n+6660)) 
      (filter (\n->(n `elem` primes) && ((n + 3330) `elem` primes) && ((n + 6660) `elem` primes)) [1..9999]))))

primes = fromList(findPrimes 9999)