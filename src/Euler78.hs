module Euler78 where
  
--takeWhile  
--npnum:: [Integer]->[Integer]
--npnum p = 

partition = length(last(takeWhile(\l -> head l `mod` 1000000 /= 0) 
  (scanl (\a _-> sum (map (\g->signum g * a!!fromInteger(abs g - 1))
  (takeWhile(\g->fromInteger(abs g - 1) < length a) pentagonal)):a) [1] [1..])))
  
pentagonal:: [Integer]
pentagonal = (\k-> (if even (k - 1) then 1 else -1) * k*(3*k-1) `div` 2) 
  . (\k-> if odd k then (1 + k) `div` 2 else - k`div`2)
  <$> [1..]  

reverse::[Integer] -> [Integer]  
reverse = foldl (\a b-> b:a) []  