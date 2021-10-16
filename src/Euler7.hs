module Euler7 where
import Euler3

findKthPrime :: Int -> Int
findKthPrime k =
  last(findKthPrimeR k [2])
findKthPrimeR :: Int -> [Int] -> [Int]
findKthPrimeR 1 l = l
findKthPrimeR k l =
  findKthPrimeR (k - 1) (l ++ [nextPrime (last l  + 1) l])
