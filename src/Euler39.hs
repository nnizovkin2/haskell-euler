module Euler39 where
  
import Euler19(third)  
import Data.List (sort)
import Data.IntMap (fromListWith, toList)
  
ans = foldl (\p c-> if snd p < snd c then c else p) (0,0) 
  (toList(fromListWith (+) (map(\(a,b,c)->(a+b+c, 1)) (filter (\t->t/=(0,0,0)) ([triangle x y 1000|x<-[1..333],y<-[1..500]])))))
triangle:: Int->Int->Int->(Int,Int,Int)
triangle a b lim = 
  if a <= b && sqrtInt (a^2 + b^2) ^ 2 == (a^2 + b^2) 
   && a + b + sqrtInt (a^2 + b^2) <= lim 
  then (a, b, sqrtInt (a^2 + b^2)) 
  else (0,0,0)   
sqrtInt=floor . sqrt . fromIntegral