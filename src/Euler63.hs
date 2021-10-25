module Euler63 where
ans = length (filter(\(d,p)->length(show (d^p)) == p)[(d,p)|d<-[1..9],p<-[1..25]])  

