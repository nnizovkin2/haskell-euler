module Euler62 where

import Data.List (sort)
import Data.Map (fromListWith, toList)


--The cube, 41063625 (3453), can be permuted to produce two other cubes: 56623104 (3843) and 66430125 (4053). 
--In fact, 41063625 is the smallest cube which has exactly three permutations of its digits which are also cube.
--
--Find the smallest cube for which exactly five permutations of its digits are cube.

ans = minimum(concatMap snd (filter(\(_,x2)->length x2>=5) (toList(fromListWith(++)
        (map (\x->((sort.show.(^3)) x,[x^3])) [1..10000])))))