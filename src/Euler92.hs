module Euler92 where

import Data.Char (digitToInt)
import Data.Set (fromList)

ans = length (filter (`elem` set) (map f [1..10000000]))
fr:: Int->Int
fr 89 = 89
fr 1 = 1
fr n = fr(f n)  
f:: Int->Int
f n = sum(map((^2).digitToInt) (show n))    
set = fromList(filter (\n->fr n == 89)  [1..7 * (9 ^ 2)])