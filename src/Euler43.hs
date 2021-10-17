module Euler43 where

import Euler41(permutations)

ans = sum(map(\s->read s::Integer)
        (filter (isDivisible [2,3,5,7,11,13,17].subLists 3.tail) (permutations['0','1','2','3','4','5','6','7','8','9'])))
isDivisible::[Int]->[[Char]]->Bool
isDivisible d n = all (\(num,divisor)->(read num::Int)`mod`divisor==0)(zip n d)
subLists:: Int->[a]->[[a]]
subLists len l = foldl (\r el->r++[tail(last r) ++ [el]]) [take len l] (drop len l)
