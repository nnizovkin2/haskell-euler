module Euler53 where
ans = fst(foldl(\(s, row) _->(\r->(s+length(filter(>1000000) r), r))(zipWith(+) row (0:row) ++ [1])) (0, [1]) [1..100])
  
