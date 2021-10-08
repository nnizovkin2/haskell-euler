module Euler15(ans) where

--Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down,
-- there are exactly 6 routes to the bottom right corner.
--
--
--How many such routes are there through a 20×20 grid?

ans::Integer

ans = 40 * 39 * 38 * 37 * 36 * 35 * 34 * 33 * 32 * 31 * 30 * 29 * 28 * 27 * 26 * 25 * 24 * 23 * 22 * 21 `div`
  (20 * 19 * 18 * 17 * 16 * 15 * 14 * 13 * 12 * 11 * 10 * 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2)