module Util where

import Data.Bits
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Monoid
import Params
import Types

import System.IO.Unsafe (unsafePerformIO)

hilbertDistance :: (Num a, Bits a, Ord a) => Int -> (a,a) -> a
hilbertDistance d (x,y)
    | x < 0 || x >= 1 `shiftL` d = error "x bounds"
    | y < 0 || y >= 1 `shiftL` d = error "y bounds"
    | otherwise = dist (1 `shiftL` (d - 1)) (1 `shiftL` ((d - 1) * 2)) 0 x y
    where dist 0 _ result _ _ = result
          dist side area result x2 y2 =
              case (compare x2 side, compare y2 side) of
              (LT, LT) -> step result y2 x2
              (LT, _)  -> step (result + area) x2 (y2 - side)
              (_, LT)  -> step (result + area * 3) (side - y2 - 1)
                          (side * 2 - x2 - 1)
              (_, _)   -> step (result + area * 2) (x2 - side) (y2 - side)
              where step = dist (side `shiftR` 1) (area `shiftR` 2)

hilbertDistanceFn :: (Integer, Integer) -> Integer
hilbertDistanceFn = hilbertDistance (10 :: Int)

hilbertDistanceOfRect :: Rect -> LHV
hilbertDistanceOfRect = hilbertDistanceFn . rectCenter


rectanglesIntersect :: Rect -> Rect -> Bool
rectanglesIntersect (Rect xl1 yl1 xh1 yh1) (Rect xl2 yl2 xh2 yh2) =
    not ((xh1 < xl2 || xl1 > xh2) || (yh1 < yl2 || yl1 > yh2))

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const . drop 1) xs (drop n xs)

a & b = b a

rectCenter :: Rect -> (Integer, Integer)
rectCenter (Rect xl yl xh yh) = (xh - xl, yh - yl)

instance Monoid Rect where
  mempty = (Rect 0 0 0 0)
  mappend (Rect xl1 yl1 xh1 yh1) (Rect xl2 yl2 xh2 yh2) = Rect (min xl1 xl2) (min yl1 yl2) (max xh1 xh2) (max yh1 yh2)

getBoundingRect :: Node -> Rect
getBoundingRect (IntNode _ rect _) = rect
getBoundingRect (LeafNode idRects) = mconcat $ map getRect idRects

buildIntNode :: [Node] -> Node
buildIntNode children = IntNode lhv rect children where
  lhv = maximum $ map getLHV children
  rect = mconcat (map getBoundingRect children)

getLHV :: Node -> LHV
getLHV (IntNode lhv _ _) = lhv
getLHV (LeafNode idRects) = maximum $ map (hilbertDistanceOfRect . getRect) idRects

{- Return true if the node is full or overflowed -}
isFull :: Node -> Bool
isFull (LeafNode idRects) = length idRects == cl
isFull (IntNode _ _ children) = length children == cn

isOverflowed :: Node -> Bool
isOverflowed (LeafNode idRects) = length idRects > cl
isOverflowed (IntNode _ _ children) = length children > cn

-- TODO: nice way to do this?
isFullOrOverflowed x = or [isFull x, isOverflowed x]

{- Get the list of Hilbert values of a Node's children.
For an interior node, this is a list of LHVs of the nodes under it.
For a leaf node, this is a list of the Hilbert values of its rectangles -}
getHilbertValues :: Node -> [Integer]
getHilbertValues (LeafNode idRects) = map (hilbertDistanceOfRect . getRect) idRects
getHilbertValues (IntNode _ _ children) = map getLHV children

isOrdered :: Ord a => [a] -> Bool
isOrdered (x:y:xs) | x<=y = isOrdered (y:xs)
                   | otherwise = False
isOrdered _ = True

isLeafNode :: Node -> Bool
isLeafNode (IntNode _ _ _) = False
isLeafNode (LeafNode _) = True

getChildRects :: Node -> [Rect]
getChildRects n@(LeafNode _) = map getRect $ getIDRects n
getChildRects (IntNode _ _ children) = map getNodeRect children

getNodeRect :: Node -> Rect
getNodeRect (LeafNode idRects) = mconcat $ map getRect idRects
getNodeRect (IntNode _ r _) = r

getLeafChildren :: Node -> [Node]
getLeafChildren (IntNode _ _ children) = filter isLeafNode children
getLeafChildren (LeafNode {}) = error "Tried to get leaf children of LeafNode"

getIntChildren :: Node -> [Node]
getIntChildren (IntNode _ _ children) = filter (not . isLeafNode) children
getIntChildren (LeafNode {}) = error "Tried to get int children of LeafNode"

{- Divide a list of items as evenly as possible into n groups -}
distributeItems :: Int -> [a] -> [[a]]
distributeItems n items = chunksOf chunkSize items where
  chunkSize = let (quot, rem) = length items `quotRem` n in
    quot + (if rem == 0 then 0 else 1)
