module Euler4 where

import Data.Void (Void)

--A palindromic number reads the same both ways. 
--The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.

--Find the largest palindrome made from the product of two 3-digit numbers.

isPalindrome :: [Char] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = 
  (x == last xs) && isPalindrome(init xs)

findMaxPalindrome :: Integer
findMaxPalindrome = findMaxPalindromeR 999 999 0
  
findMaxPalindromeR :: Integer -> Integer -> Integer -> Integer  
findMaxPalindromeR _ 899 m = m
findMaxPalindromeR 899 x m = findMaxPalindromeR 999 (x - 1) m
findMaxPalindromeR x y m =
  if isPalindrome(show (x * y)) && x * y > m then findMaxPalindromeR (x - 1) y (x * y)
  else findMaxPalindromeR (x - 1) y m   
   
      
  
