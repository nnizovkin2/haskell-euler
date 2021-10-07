module Euler10 where
  
import Euler3  
  
--The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
--
--Find the sum of all the primes below two million.  
--142913828922
ans:: Integer
ans = sum(findPrimes [] 2) 

findPrimes:: [Integer] -> Integer -> [Integer]
findPrimes l 2000000 = l
findPrimes l n = 
  if isPrime(n, l) then findPrimes (l ++ [n]) (n + 1) 
  else findPrimes l (n + 1)
  
  