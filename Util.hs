module Util where

import Types
import Data.Bits
import Data.List (foldl')

hilbertDistance :: (Num a, Bits a, Ord a) => Int -> (a,a) -> a
hilbertDistance d (x,y)
    | x < 0 || x >= 1 `shiftL` d = error "x bounds"
    | y < 0 || y >= 1 `shiftL` d = error "y bounds"
    | otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
    where dist 0 _ result _ _ = result
          dist side area result x y =
              case (compare x side, compare y side) of
              (LT, LT) -> step result y x
              (LT, _)  -> step (result + area) x (y - side)
              (_, LT)  -> step (result + area * 3) (side - y - 1)
                          (side * 2 - x - 1)
              (_, _)   -> step (result + area * 2) (x - side) (y - side)
              where step = dist (side `shiftR` 1) (area `shiftR` 2)

rectanglesIntersect :: Rect -> Rect -> Bool
rectanglesIntersect (Rect xl1 yl1 xh1 yh1) (Rect xl2 yl2 xh2 yh2) =
    not ((xh1 < xl2 || xl1 > xh2) || (yh1 < yl2 || yl1 > yh2))

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const .drop 1) xs (drop n xs)

a & b = b a
