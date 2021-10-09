module Euler22 where
  
import Data.List.Split
import Data.List

--Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names,
-- begin by sorting it into alphabetical order. 
-- Then working out the alphabetical value for each name,
--  multiply this value by its alphabetical position in the list to obtain a name score.
--
--For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53,
-- is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.
--
--What is the total of all the name scores in the file?  

--  print(fromEnum 'A')
--splitOn "," "my,comma,separated,list"
--["my","comma","separated","list"]  

totalScore:: String->Integer
totalScore s = sum(zipWith score [1..length(names s)] (names s))

names:: String->[String]
names s = sort (splitOn "," (filter (/='"') s))

score:: Int->String->Integer
score i s = toInteger(i * sum (map (\ch -> 1 + fromEnum ch - fromEnum 'A') s))