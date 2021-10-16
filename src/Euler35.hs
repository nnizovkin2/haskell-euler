module Euler35(a,primes,allRotations,dn) where

import Euler10
import Data.IntSet (fromList,member)

a:: [Int]
a = filter(all(`member` primes) . allRotations)[1..999999]  
allRotations::Int->[Int]
allRotations n = if n `mod` 10 == 0 then [n] else rotateTill (rotate n) n
rotateTill::Int->Int->[Int]
rotateTill n lim = if n == lim || n `mod` 10 == 0 then [n] else n:rotateTill (rotate n) lim  

dn:: Int->Int
dn n  
  | n>=10^5 = 5
  | n>=10^4 = 4
  | n>=10^3 = 3
  | n>=10^2 = 2
  | n>=10^1 = 1
  | otherwise = 0
rotate:: Int->Int
rotate n = n `div` 10 + n `mod` 10 * 10 ^ dn n 
primes = fromList(findPrimes 1000000)
 