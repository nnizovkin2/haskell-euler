module Euler56 where
ans = maximum(map snd (foldl(\l _->zipWith (\(v, m) n -> (\d-> if ds d > m then (d, ds d) else(d, m))(v * n)) l [1..100]) 
      (map (const (1, 0)) [1..100]) [1..100]))   

ds:: Integer->Int
ds 0 = 0
ds n = fromInteger (n `mod` 10) + ds (n `div` 10)  