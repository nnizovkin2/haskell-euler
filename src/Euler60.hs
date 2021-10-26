module Euler60 where
import Euler10
import Euler58(isPrime)
import Data.Set(fromList)
import Data.List (sort, nub)
    
--Prototype. Calc in Java.     
ans = foldl(\cl _-> nub(map (\(c, ve)->sort(c ++ [ve]))(filter(\(c, ve)->notElem ve c && 
                                              all (\(cv1, cv2)->cv1==cv2 || isC cv1 cv2 ve) 
                                              [(cv1,cv2)|cv1<-c, cv2<-c]) [(c, ve)|c<-cl, ve<-v])))
      e [1..3]
isC::Int->Int->Int->Bool
isC x y z = isE x y && isE x z && isE y z

isE:: Int->Int->Bool
isE x y = (x,y) `elem` es || (y,x) `elem` es

es = fromList ep   
e = map(\(x,y)->[x,y]) ep
ep = filter(\(x,y)->x<y && isEdge x y)((\p->[(x,y)|x<-p,y<-p]) v)  

v = findPrimes 10000  

isEdge:: Int->Int->Bool
isEdge a b = (\(l1, l2)->isGP l1 l2 && isGP l2 l1)(show a, show b)  

isGP::[Char]->[Char]->Bool
isGP a b = isPrime(read(a ++ b)::Integer) 
     