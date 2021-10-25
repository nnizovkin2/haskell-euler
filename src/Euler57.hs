module Euler57 where
  
ans = fst(foldl(\(r,(p,q))_->(\(pt,qt)->if length (show (3*q + p)) > length (show (2*q + p)) 
                              then (r+1, (pt,qt)) 
                              else (r, (pt,qt)))(q,2*q + p))(0,(0,1))[1..1000])  

