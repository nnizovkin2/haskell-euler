module Lib where

countMult :: Integer -> Integer
countMult x = countMultReq(x - 1)

countMultReq :: Integer -> Integer
countMultReq x =
  if x == 0 then 0
  else isDiv x + countMultReq (x-1)

isDiv :: Integer -> Integer
isDiv x =
  if mod x 5 == 0 || mod x 3 == 0 then x
  else 0
    
--findPrimes l n = findPrimes (addToListIfNeed l n) (n + 1)
--  
--addToListIfNeed:: [Integer] -> Integer -> [Integer]   
--addToListIfNeed l n = 
--  if isPrime(n, l) then l ++ [n]
--  else l    