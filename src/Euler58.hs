module Euler58 where
  
import Data.Set (fromList)
import Data.Foldable (foldlM)
import Euler39

ratio = 
  (\(_, t, _)->(t `div` 2) + 1)
        (either id id (foldlM(\p _-> e (gen p)) (0, 1, [1, 1, 1, 1]) [1 ..]))  
ans = gen (0, 1, [1, 1, 1, 1])
gen:: (Integer, Integer, [Integer])->(Integer, Integer, [Integer])
gen (p, t, ar) = (\l->(toInteger(length(filter isPrime l)) + p, t + 4, l)) (zipWith(\i v-> v + (i + t - 1) * 2) [1..4] ar)
e::(Integer, Integer, [Integer])-> Either (Integer, Integer, [Integer]) (Integer, Integer, [Integer])
e (p, t, ar) =
  if p * 10 < t
    then Left (p, t, ar)
    else Right (p, t, ar)

isPrime:: Integer->Bool
isPrime n = isPrimeR n (toInteger(floor(sqrt(fromInteger  n)))) 2   
isPrimeR:: Integer->Integer->Integer->Bool
isPrimeR n lim cur = (cur > lim) || ((n `mod` cur) /= 0 && isPrimeR n lim (cur + 1))