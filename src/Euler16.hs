module Euler16 where
import Data.Char(digitToInt)

--215 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
--
--What is the sum of the digits of the number 21000?  

ans::Int

ans = sum(map digitToInt (show(2^1000))) 
