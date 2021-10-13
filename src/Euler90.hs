module Euler90 where

import Data.List(nub,nubBy,sort,isPrefixOf, (\\), filter, map)
import Data.Bifunctor(bimap)

ans = length(nub (concatMap alignPToCube clearedList))
clearedList :: [([Int], [Int])]
clearedList = concatMap alignPToCube 
                (concatMap addNineToPair
                 (filter (\el1 -> not(any(\el2 -> isPref el2 el1 && (el1 /= el2)) list)) list))
list = nubBy(\p1 p2->p1 == p2 || flipIf True p1 == p2) 
         (filter (\p->length(fst p) <= 6 && length(snd p) <= 6)
           (map (Data.Bifunctor.bimap (sort . nub) (sort . nub) . (\c->unzip(zipWith flipIf c s))) (nCartesian 9)))
           
alignPToCube::([Int],[Int])->[([Int],[Int])]
alignPToCube (c1, c2) = [sortT(sort x, sort y)|x<-alignToCube c1, y<-alignToCube c2]
sortT::Ord a=>(a,a)->(a,a)
sortT (a, b) = if a > b then (b, a) else (a, b)
alignToCube::[Int]->[[Int]]
alignToCube l = if length l == 6 then [l]
                else foldl (\l2 _ -> addDigitL l2) [l] [1..(6-length l)]

addDigitL::[[Int]]->[[Int]]
addDigitL = concatMap addDigit               
addDigit::[Int]->[[Int]]
addDigit l = map (\d->l++[d]) ([1..9] Data.List.\\ l)               

addNineToPair::([Int],[Int])->[([Int],[Int])] 
addNineToPair (l1, l2) = [(x,y)| x<-removeD (addNineL l1), y<-removeD (addNineL l2)]
removeD::Eq a=>([a],[a])->[[a]]
removeD(l1, l2) = if l1 == l2 then [l1] else [l1, l2]
addNineL::[Int]->([Int],[Int])
addNineL l = unzip [(x, if x == fst y then snd y else x)| x<-l, y<-[(6,9)]]

isPref::Eq a => ([a], [a]) -> ([a], [a]) -> Bool
isPref (a0, a1) (b0, b1) = (isPrefixOf a0 b0 && isPrefixOf a1 b1) || (isPrefixOf a0 b1 && isPrefixOf a1 b0)            
  
s = [(0,1),(0,4),(0,6),(1,6),(2,5),(3,6),(4,6),(6,4),(8,1)]   
 
flipIf::Bool->(a,a)->(a,a)
flipIf b (a0,a1) = if b then (a1, a0) else (a0, a1)

nCartesian:: Int->[[Bool]]
nCartesian n = map (alignTo n False . cartesian) [0..2^n - 1]

alignTo:: Int->a->[a]->[a]
alignTo n c l = [c|_<-[1..(n - length l)]] ++ l   

cartesian:: Integer->[Bool]
cartesian n = reverse (cartesianR n)
cartesianR:: Integer->[Bool]
cartesianR 0 = [False]
cartesianR 1 = [True]
cartesianR n = (n `mod` 2==1):cartesian(n `div` 2)     