module Euler30 where

import Data.Char (digitToInt)

  
ans =  sum (filter (\i-> i == sum ((^5) . digitToInt <$> show i)) [2..354294])  

