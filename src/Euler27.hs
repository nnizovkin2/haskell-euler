module Euler27 where

import Euler10
import Euler3
import Data.Set(fromList)
--n^2 + a * n + b  
ans = maximum[(pseq(a,b), a * b)|a<-[-1000..1000],b<-[-1000..1000]]

pseq(a, b) = length(takeWhile(`elem` primes) [toInteger(x^2 + a*x + b)|x<-[0..]])

primes = fromList(findPrimes 20100)
