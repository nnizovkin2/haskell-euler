module Euler3(maxPrimeNumber) where

--The prime factors of 13195 are 5, 7, 13 and 29.

--What is the largest prime factor of the number 600851475143 ?

isPrime :: (Integer, [Integer]) -> Bool   
isPrime (_, []) = True 
isPrime (pc, x:xs) =
  mod pc x /= 0 && isPrime(pc, xs) 

nextPrime :: (Integer, [Integer]) -> Integer 
nextPrime (pc, l) = 
  if isPrime(pc, l) then pc
  else nextPrime (pc + 1, l)
    
maxPrimeNumber :: Integer -> Integer
maxPrimeNumber num =
  maxPrimeNumberR(num, 0, [2])


maxPrimeNumberR :: (Integer, Integer, [Integer]) -> Integer
maxPrimeNumberR (number, max, l)
  | number == 1 = max
  | mod number (last l) == 0 = maxPrimeNumberR(number `div` last l, last l, l)
  | otherwise = maxPrimeNumberR(number, max, l ++ [nextPrime(last l + 1, l)])   
  