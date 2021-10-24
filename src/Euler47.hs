module Euler47 where
  
  

--The first two consecutive numbers to have two distinct prime factors are:
--
--14 = 2 × 7
--15 = 3 × 5
--
--The first three consecutive numbers to have three distinct prime factors are:
--
--644 = 2² × 7 × 23
--645 = 3 × 5 × 43
--646 = 2 × 17 × 19.
--
--Find the first four consecutive integers to have four distinct prime factors each. What is the first of these numbers?    

import Euler12
import Data.List(nub)
--only code. get res in java
ans = filter(all (\n->length(nub(getPrimes n)) == 4))(subLists 4 [1000..999999])

subLists:: Int->[a]->[[a]]
subLists s (x:xs) = if length xs == (s - 1) then [x:xs]
                  else (x:take (s - 1) xs):subLists s xs  
