module Euler10 where
  
import Euler3  
  
--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
--Find the sum of all the primes below two million.  
--142913828922
ans:: Integer
ans = foldl(\s c->s+toInteger c) 0 (findPrimes 2000000) 

findPrimes:: Int -> [Int]
findPrimes = findPrimesC [] 2
findPrimesC:: [Int] -> Int -> Int -> [Int]
findPrimesC l cur lim
  | cur == lim = l
  | isPrime l cur = findPrimesC (l ++ [cur]) (cur + 1) lim
  | otherwise = findPrimesC l (cur + 1) lim
  
  