module Euler14 where

--The following iterative sequence is defined for the set of positive integers:
--
--n → n/2 (n is even)
--n → 3n + 1 (n is odd)
--
--Using the rule above and starting with 13, we generate the following sequence:
--
--13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
--It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. 
-- Although it has not been proved yet (Collatz Problem), it is thought that all starting numbers finish at 1.
--
--Which starting number, under one million, produces the longest chain?
--
--NOTE: Once the chain starts the terms are allowed to go above one million.  
  
maxLengthEl:: Integer -> (Integer, Integer)
maxLengthEl n = snd(maxLengthR(n, (-1, 0)))    
maxLengthR:: (Integer, (Integer, Integer)) -> (Integer, (Integer, Integer)) 
maxLengthR (0, (i, m)) = (0, (i, m))
maxLengthR (c, (i, m)) = if m < len c then maxLengthR(c - 1, (c, len c)) else maxLengthR(c - 1, (i, m))
len:: Integer -> Integer
len n = snd(lenR(n, 1)) 
lenR:: (Integer, Integer) -> (Integer, Integer)
lenR(1, l) = (1, l)
lenR(e, l) = lenR(next e, l + 1) 
  
next:: Integer -> Integer
next n =
  if even n then n `div` 2
  else 3 * n + 1      