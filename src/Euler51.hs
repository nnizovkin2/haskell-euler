module Euler51 where
  
import Euler10  
import Data.IntSet (fromList, member)
import Data.List (sort)
import Data.Char (digitToInt)
  
ans = gen 6 2 ++ gen 6 3 ++ gen 6 4 
gen:: Int->Int->[[Int]]
gen n k = filter (\l->length l > 7) [filter (`member` set) (replace n x y)|x<-[0..10^(n-k) - 1], y<-combination n k]
replace::Int->Int->[Int]->[Int]
replace n p pos = map (genNum n p pos) (if n `elem` pos then [1..9] else [0..9])
genNum::Int->Int->[Int]->Int->Int
genNum n p pos d = fst(foldl(\(r, pn) dp->
                    if dp `elem` pos then (r * 10 + d, pn)
                    else if not (null pn) then (r * 10 + head pn, tail pn)
                    else (r*10, pn))(0, map digitToInt (show p)) (reverse [1..n]))   
combination::Int->Int->[[Int]]
combination _ 0 = [[]]
combination 0 _ = []
combination n k = concatMap (\v->map (v :) (combination (v - 1) (k - 1))) (reverse [1..n])

set = fromList(findPrimes 1000000)