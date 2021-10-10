module Main where

import Data.BigDecimal
import Euler2 (fibSeq)
import Euler78
import Control.Monad (liftM2, mfilter)
import Control.Applicative (liftA2, liftA3)
import Control.Monad.Omega

main :: IO ()
main = do
  print(partition)
