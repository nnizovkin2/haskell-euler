module Euler88 where
  
import Euler12  
import Euler58
import Data.Function ((&))
import Data.List (nub)
import Data.IntSet(fromList, size, delete, IntSet, null)
import Data.Foldable (foldlM)

--  

ans = sum (fst(eIdId (foldlM(\(l, s) (n, lOfD) -> (\rs->if any(\i->size s /= size (delete i s)) lOfD then es(l ++ [n], rs)
        else es(l, rs))(foldl(flip delete) s lOfD)) ([], fromList[2..12000])
        (map ((\(x:xs)->(head x, map (\l->(head x - sum l) + length l) xs))
        .foldl(\listOfLists n -> nub(concatMap(\dividerList->(\zl->map(\i->change i (fst(zl!!i)*n) dividerList)
        (filter (\i->uncurry (/=) (zl!!i))[0..(length zl - 1)]) ++ [dividerList ++ [n]])
        (zip dividerList (0:dividerList))) listOfLists)) [[]] . getPrimes) [2..]))))

es::([Int], IntSet)-> Either ([Int], IntSet) ([Int], IntSet)
es (l, s) =
  if Data.IntSet.null s
    then Left (l, s)
    else Right (l, s)

change::Int->a->[a]->[a]
change pos el l = take pos l ++ changeFirst el (drop pos l)   

changeFirst::a->[a]->[a]
changeFirst el (_:xs) = el:xs

s::IntSet
s = fromList [1..12000]