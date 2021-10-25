module Euler52 where

import Data.List (sort)

  
ans = minimum(filter(\n->all(\m->sort (show n)==sort(show(n * m)))[2..6])[1..1000000])  

