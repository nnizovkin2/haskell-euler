module Euler40 where

import Data.Char (digitToInt)

ans= a!!0*a!!9*a!!99*a!!999*a!!9999*a!!99999*a!!999999
a= map digitToInt (concatMap show [1..200000])  

