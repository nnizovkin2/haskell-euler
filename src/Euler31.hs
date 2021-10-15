module Euler31 where
  
--1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
  
ans= (next 200 . next 100 . next 50 . next 20 . next 10 . next 5 . next 2) (0:(1 <$ [0..200]))

next::Int->[Integer]->[Integer]
next v l = foldl (\lt el -> if el - v >= 0 then lt ++ [l!!el + lt!!(el - v)] else lt ++ [l!!el]) [] [0..length l - 1]  

