module Euler36 where
  
import Numeric(showIntAtBase)
import Euler4
import GHC.Show (intToDigit)

ans = sum(filter(\n->isPalindrome(show n) && isPalindrome (showIntAtBase 2 intToDigit n ""))[1..1000000])
