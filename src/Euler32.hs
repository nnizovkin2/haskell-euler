module Euler32 where

import Data.List ((\\), nub)

  
--We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once;
-- for example, the 5-digit number, 15234, is 1 through 5 pandigital.
--
--The product 7254 is unusual, as the identity, 39 × 186 = 7254, containing multiplicand, multiplier, and product is 1 through 9 pandigital.
--
--Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
--
--HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.  

ans:: Int
ans= sum (nub(map (uncurry (*)) (filter isPandigital([(x,y)|x<-[2..9], y<-[1000..5000]]
  ++[(x,y)|x<-[10..99], y<-[100..999]]))))
isPandigital::(Int,Int)->Bool
isPandigital (a,b) = (\s->length s == 9 && null (s\\['1','2','3','4','5','6','7','8','9'])) (show a ++ show b ++ show (a * b))
toStr::Int->Int->String
toStr a b = show a ++ show b ++ show (a * b)