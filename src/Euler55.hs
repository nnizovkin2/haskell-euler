module Euler55 where
  
import Euler4  

ans = length (filter (isLychrel 50) [0..10000])
isLychrel::Int->Integer->Bool  
isLychrel 0 _ = True
isLychrel c n = (\nn-> not (isPalindrome (show nn)) && isLychrel (c- 1) nn)(n + revert n) 

  
revert:: Integer->Integer  
revert n = read(reverse(show n))::Integer  