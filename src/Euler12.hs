module Euler12 where
  
import Euler3
import Data.Map (fromListWith, toList)

getListOfPrimes :: Integer -> Integer
getListOfPrimes 50000 = 0
getListOfPrimes n =
  if product ([(+ 1) v | (k, v) <- toList (fromListWith (+) [(c, 1) | c <- getPrimes (n * (n + 1) `div` 2)])]) > 500
    then n * (n + 1) `div` 2
    else getListOfPrimes (n + 1)
  
getPrimes :: Integer -> [Integer]
getPrimes n = tail(getPrimesR(n,2,[2]))  
getPrimesR :: (Integer, Integer, [Integer]) -> [Integer]
getPrimesR (number, prime, l) 
  | number == 1 = l
  | mod number prime == 0 = getPrimesR (number `div` prime, prime, l ++ [prime])
  | otherwise = getPrimesR(number, nextPrime (prime + 1) l, l)  

