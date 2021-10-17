module Euler41(a) where

import Euler39
--import Euler78M
import Data.List (delete, find)
import Data.Maybe

a:: Integer
a = maybe 0 read (find (\p->isPrime(read p::Integer)) (permutations['7','6','5','4','3','2','1']))
permutations:: [Char]->[[Char]]
permutations [] = [[]]
permutations l = concatMap (\el->map(el:)(permutations(delete el l))) l

isPrime:: Integer->Bool
isPrime n = not (any (\el->n`mod`el==0) [2..(floor(sqrt(fromInteger n)::Float))])
