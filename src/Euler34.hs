module Euler34 where

ans = sum (filter(\n->s n==n) [4..999999])  
s::Int->Int
s 0 = 0
s n = df (n `mod` 10) + s (n `div` 10)      
df:: Int->Int
df d
  | d==0 = 1
  | d==1 = 1
  | d==2 = 2
  | d==3 = 6  
  | d==4 = 24  
  | d==5 = 120
  | d==6 = 720
  | d==7 = 5040  
  | d==8 = 40320  
  | otherwise = 362880  