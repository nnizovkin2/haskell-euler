module Euler7(findKthPrime) where
import Euler3

findKthPrime :: Integer -> Integer
findKthPrime k =
  last(findKthPrimeR k [2])
findKthPrimeR :: Integer -> [Integer] -> [Integer]
findKthPrimeR 1 l = l
findKthPrimeR k l =
  findKthPrimeR (k - 1) (l ++ [nextPrime (last l  + 1) l])
