module Euler21 where
  
import Euler19(third)  

--Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
--If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.
--
--For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284.
--The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
--
--Evaluate the sum of all the amicable numbers under 10000    

ans :: Integer
ans = snd(sumR(1, 0))

sumR::(Integer, Integer) -> (Integer, Integer)
sumR(10000, s) = (10000, s)
sumR (n, s) =
  if n < getSumDivisors n 
  && n == getSumDivisors(getSumDivisors n)
  && n < 10000
  && getSumDivisors n < 10000
  then sumR(n + 1, s + n + getSumDivisors n)
  else sumR(n + 1, s) 

getSumDivisors:: Integer->Integer
getSumDivisors n = sum(third(getDivisorsR (n, 1, []))) 

getDivisorsR:: (Integer, Integer, [Integer]) -> (Integer, Integer, [Integer])
getDivisorsR (n, d, l)
  | n < d * 2 = (n, d, l)
  | n `mod` d == 0 = getDivisorsR(n, d + 1, l ++ [d])
  | otherwise = getDivisorsR(n, d + 1, l)

