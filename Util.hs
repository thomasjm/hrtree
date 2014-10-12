module Util where

import Params
import Types
import Data.Bits
import Data.List (foldl')

import Zora.Graphing.DAGGraphing


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

boundingRect :: [Rect] -> Rect
boundingRect (x:xs) = foldl' f x xs where
  f (Rect xl1 yl1 xh1 yh1) (Rect xl2 yl2 xh2 yh2) = Rect (min xl1 xl2) (min yl1 yl2) (max xh1 xh2) (max yh1 yh2)

getBoundingRect :: Node -> Rect
getBoundingRect (IntNode _ rect _) = rect
getBoundingRect (LeafNode idRects) = boundingRect $ map getRect idRects


buildIntNode :: [Node] -> Node
buildIntNode children = IntNode lhv rect children where
  lhv = maximum $ map getLHV children
  rect = boundingRect (map getBoundingRect children)

getLHV :: Node -> LHV
getLHV (IntNode lhv _ _) = lhv
getLHV (LeafNode idRects) = maximum $ map (hilbertDistanceOfRect . getRect) idRects

{- Return true if the node is full or overflowed -}
isFull :: Node -> Bool
isFull (LeafNode idRects) = length idRects >= cl
isFull (IntNode _ _ children) = length children >= cn

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

instance DAGGraphable Node where
  expand (LeafNode idRects) = Just (Just $ show idRects, [])
  expand (IntNode lhv rect children) = Just (Just $ show (lhv, rect), map f children) where
    f n@(LeafNode _) = (Just $ show $ maximum $ getHilbertValues n, n)
    f n@(IntNode lhv _ _) = (Just $ show lhv, n)
