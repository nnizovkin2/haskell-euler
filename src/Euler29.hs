module Euler29 where

import Data.List ((\\))


ans=28 + 26 + 40 + 52 + 83 + (99 - 49 - 16) + (99 - 49 - 17) + (99 - 25)
    + (99 - 49) * 4 + 99 * (99 - 12)
test= (([2..100] \\ ((*2)<$>[2..100])) \\ ((*3)<$>[2..100])) \\ ((*5)<$>[2..100])
num= length((((*1)<$>[2..100]) \\ ((*2)<$>[2..100])) \\ ((*3)<$>[2..100]))


