module Euler42 where
  
import Data.List
import Euler22(words)  
import Data.Char (ord)

twn:: [Char]->Int
twn s = length(filter (contains tseq) (map (sum . map (\l->ord l - ord 'A' + 1)) (Euler22.words s)))
contains:: [Int]->Int->Bool
contains (el:xs) v = (v == el) || (v > el && contains xs v)           
tseq :: [Int]
tseq = map(\n->n*(n+1) `div` 2)[1..]         
         