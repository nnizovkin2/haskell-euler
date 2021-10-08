module Euler13(sumOfInts) where

readInt:: String->Integer
readInt s = read s::Integer 
 
sumOfInts::[String] -> Integer
sumOfInts l = read(take 10 (show(sum (map readInt l)))) 
