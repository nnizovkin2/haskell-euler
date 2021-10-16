module Euler37 where

import Euler35(dn)  
import Euler10
import Data.IntSet (fromList,member)
--The number 3797 has an interesting property. Being prime itself, it is possible to continuously 
--remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. 
--Similarly we can work from right to left: 3797, 379, 37, and 3.
--
--Find the sum of the only eleven primes that are both truncatable from left to right and right to left.
--
--NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

ans=sum(filter(\n->all(`member` primes)(pref n ++ suff n))[10..1000000])
pref::Int->[Int]
pref n = if n < 10 then [n] else n:pref(n`div`10) 

suff::Int->[Int]
suff n = if n < 10 then [n] else n:suff(n -  (10 ^ dn n) * (n `div` (10 ^ dn n))) 
primes = fromList(findPrimes 1000000)


