module Euler9 where
  
ans :: Integer
ans = findMultR 1 1  

findMultR :: Integer -> Integer -> Integer
findMultR 500 b = findMultR 1 (b + 1)
findMultR _ 500 = -1
findMultR a b = 
  if a ^ 2 + b ^ 2 == (1000 - a - b) ^ 2 then a * b * (1000 - a - b)
  else findMultR (a + 1) b   

