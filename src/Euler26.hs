module Euler26 where

import Data.BigDecimal
import Data.Text
import Data.Text.Conversions
import Data.Text.Internal.Search

--divide ((BigDecimal 1 0), (BigDecimal 257 0)) (halfUp 1000)

maxPeriodFraction :: (Int, Int)
maxPeriodFraction =
  Prelude.foldr
    (\(i1, s1) (i2, s2) -> if s1 > s2 then (i1, s1) else (i2, s2))
    (0, 0)
    (Prelude.zip [1 .. 999] (Prelude.map period [1 .. 999]))

period :: Integer -> Int
period n =
  Prelude.head
    ( indices
        (Data.Text.take 50 (Data.Text.drop 50 (fraction n 110)))
        (Data.Text.drop 100 (fraction n 2000))
    ) + 50

fraction :: Integer -> Integer -> Text
fraction n scale = convertText (toString (divide (BigDecimal 1 0, BigDecimal n 0) (halfUp scale)))
