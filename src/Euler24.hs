module Euler24 where
import Data.List
  
import Euler20(f)  
import Euler19(forth)  

toFactorialNotation :: Integer -> [Int]
toFactorialNotation n = forth(toFactorialNotationR(n, 9, [0,1,2,3,4,5,6,7,8,9], []))

toFactorialNotationR:: (Integer, Integer, [Int], [Int]) -> (Integer, Integer, [Int], [Int])   

toFactorialNotationR(0, d, rest, res) = (0, d, rest, res ++ rest)
toFactorialNotationR(n, 0, rest, res) = (n, 0, rest, res ++ rest)
toFactorialNotationR(n, d, rest, res) =
 toFactorialNotationR(n `mod` f d, d - 1, remove (toInt(n `div` f d)) rest, res ++ [rest !! toInt(n `div` f d)])
 
remove:: Int->[a]->[a]
remove i l = take i l ++ drop (1 + i) l 

toInt:: Integer->Int
toInt i = fromInteger i::Int

fromList::[Int]->Integer
fromList = foldl (\s c -> s * 10 + toInteger c) 0
