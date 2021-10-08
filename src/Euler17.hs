module Euler17 where

import Data.Char(isSpace)

--ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen
--one two three four five six seven eight nine
--twenty thirty forty fifty sixty seventy eighty ninety

digits :: [[Char]]
digits = ["one","two","three","four","five","six","seven","eight","nine"]
tens :: [[Char]]
tens = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]
secondTens :: [[Char]]
secondTens = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
oneThousand :: [Char]
oneThousand = "one thousand"

len:: [String] -> Int
len l = sum(map length l)

cutWhitespaces :: [Char] -> [Char]
cutWhitespaces = filter (not . isSpace)

lettersSize:: Int
lettersSize = len digits * ((9 * 10) + 100)
 + len secondTens * 10
  + len tens * 100
   + length "hundred" * 900
    + length "and" * 99 * 9
     + length(cutWhitespaces oneThousand)