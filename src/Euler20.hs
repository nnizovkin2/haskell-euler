module Euler20 where
import Euler16
--n! means n × (n − 1) × ... × 3 × 2 × 1
--
--For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
--and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
--
--Find the sum of the digits in the number 100!

f:: Integer->Integer
f 0 = 1
f 1 = 1
f n = n * f (n - 1)

s=dSum (f 100)