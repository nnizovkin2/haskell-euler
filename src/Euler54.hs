module Euler54 where

import Data.List (sort, nub)
import Data.Map (fromListWith, toList)
import Data.Char (isDigit, digitToInt)
import Data.List.Split (splitOn)

res::String->Int
res s = length(filter(\(f, sd) -> prepareHand f > prepareHand sd)(map hands (lines s)))
hands:: String->([String], [String])
hands s = splitAt 5 (splitOn " " s)

prepareHand :: [String] -> (Int, [(Int, Int)])
prepareHand = combination.splitVS

combination::([Int], [Char])->(Int, [(Int,Int)])
combination (v, s) = (\gv->(cv v s gv, gv)) (group v)
splitVS:: [String]->([Int], [Char])
splitVS cs = unzip (map (\c->(value(head c), last c)) cs)

value:: Char->Int
value ch
  | isDigit ch = digitToInt ch
  | ch == 'T' = 10
  | ch == 'J' = 11
  | ch == 'Q' = 12
  | ch == 'K' = 13
  | otherwise = 14

cv::[Int]->[Char]->[(Int,Int)]->Int
cv v s g
  | isStraightFlush v s = 9
  | fst (head g) == 4 = 8
  | fst (head g) == 3 && fst(head (tail g)) == 2 = 7
  | isFlush s = 6
  | isStraight v = 5
  | fst (head g) == 3 = 4
  | fst (head g) == 2 && fst(head (tail g)) == 2 = 3
  | fst (head g) == 2 = 2
  | otherwise = 1

isStraight::[Int]->Bool
isStraight v = (\sl-> all(\(c1,c2)->c1-c2 == 1) (zip sl ((head sl - 1):init sl)))(sort v)

isFlush::[Char]->Bool
isFlush s = length(nub s) == 1

isStraightFlush::[Int]->[Char]->Bool
isStraightFlush v s = isStraight v && isFlush s

group::[Int]->[(Int,Int)]
group v = reverse(sort(map (\(k,vl)->(vl,k)) (toList (fromListWith (+) [(k,vl)|k<-v, vl<-[1]]))))