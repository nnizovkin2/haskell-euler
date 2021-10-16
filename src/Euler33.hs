module Euler33 where

ans = filter (uncurry isCurious) [(x,y)|x<-[10..99], y<-[10..99]]  

isCurious::Integer->Integer->Bool
isCurious a b = a * (b `mod` 10) == b * (a `div` 10) && (a `mod` 10) == (b `div` 10) && a /= b