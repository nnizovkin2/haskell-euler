module Euler38 where

import Data.List ((\\))
ans=maximum(map (\s->read s::Int)(filter isPandigit (map(pandigit[1,2]) [5000..9999]) ++ map(pandigit[1,2,3]) [100..333]))
pandigit:: [Integer]->Integer->[Char]
pandigit l d = concatMap (show.(*d)) l
isPandigit:: [Char]->Bool
isPandigit l = (length l == 9) && null (l \\ ['1','2','3','4','5','6','7','8','9'])