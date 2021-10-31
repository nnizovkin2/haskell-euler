module Euler78M where

import Euler58  
import Data.Foldable (foldlM)

ans = length (partitions (\n -> n `mod` 1000000 == 0)) - 1

partitions::(Integer -> Bool) -> [Integer]
partitions stopCriteria = eIdId (foldlM(\p _-> stopEither stopCriteria (nextPartition p)) [1] [1 ..])

stopEither::(a -> Bool) -> [a] -> Either [a] [a]
stopEither c l =
  if c (head l)
    then Left l
    else Right l

nextPartition::[Integer] -> [Integer]
nextPartition l =
  sum
    ( map
        (\g -> signum g * l !! fromInteger (abs g - 1))
        (takeWhile (\g -> fromInteger (abs g - 1) < length l) pentagonal)
    ) :
  l

pentagonal::[Integer]
pentagonal =
  (\k -> (if even (k - 1) then 1 else -1) * k * (3 * k -1) `div` 2)
    . (\k -> if odd k then (1 + k) `div` 2 else - k `div` 2)
    <$> [1 ..]
