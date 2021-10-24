module Euler65 where

import Data.Char (digitToInt)

ans = sum (map digitToInt (show(fst(foldl(\(p,q) n->plus (n, 1) (q, p)) (last s, 1) (tail(reverse s))))))  

plus::(Integer, Integer)->(Integer, Integer)->(Integer, Integer)
plus(p1, q1) (p2, q2)=(p1 * q2 + q1 * p2, q1 * q2)  

s= [2,1,2] ++ map(\n->if n `mod` 3 /= 0 then 1 else 2 * (n `div` 3 + 1))[1..97] 

