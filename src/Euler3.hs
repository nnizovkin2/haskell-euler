module Euler3 where

--The prime factors of 13195 are 5, 7, 13 and 29.

--What is the largest prime factor of the number 600851475143 ?

isPrime:: [Int]->Int->Bool
isPrime [] _ = True
isPrime (x:xs) pc =
  mod pc x /= 0 && isPrime xs pc

nextPrime:: Int->[Int]->Int
nextPrime pc l =
  if isPrime l pc then pc
  else nextPrime (pc + 1) l

maxPrimeNumber:: Int->Int
maxPrimeNumber num =
  maxPrimeNumberR(num, 0, [2])


maxPrimeNumberR:: (Int, Int, [Int])->Int
maxPrimeNumberR (number, max, l)
  | number == 1 = max
  | mod number (last l) == 0 = maxPrimeNumberR(number `div` last l, last l, l)
  | otherwise = maxPrimeNumberR(number, max, l ++ [nextPrime (last l + 1) l])
