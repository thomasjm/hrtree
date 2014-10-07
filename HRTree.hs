module HRTree where

import Data.Bits
import Data.List (foldl')

-- Understand Z-order, Hilbert curve, and Gray-code curve

cl = 10
cn = 10


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



type LHV = Integer
type ID = String
data Rect = Rect Integer Integer Integer Integer deriving (Show, Eq)
data IDRect = IDRect { getRect :: Rect, getId :: ID } deriving (Show, Eq)
data Node = IntNode LHV Rect [Node] | LeafNode [IDRect] deriving (Show, Eq)


---- Utility functions ----
rectanglesIntersect :: Rect -> Rect -> Bool
rectanglesIntersect (Rect xl1 yl1 xh1 yh1) (Rect xl2 yl2 xh2 yh2) =
    not ((xh1 < xl2 || xl1 > xh2) || (yh1 < yl2 || yl1 > yh2))

isFull (LeafNode idRects) = length idRects == cl
isFull (IntNode _ _ children) = length children == cn

lastN :: Int -> [a] -> [a]
lastN n xs = foldl' (const .drop 1) xs (drop n xs)

---------------------------


type RTree = Node

search :: Rect -> Node -> [ID]
search w (LeafNode idRects) = map getId $ filter ((rectanglesIntersect w) . getRect) idRects
search w (IntNode _ _ children) = concat $ map (search w) children

a & b = b a

data Cxt = Top | Descend (LHV, Rect) [Node] Cxt [Node] deriving (Show, Eq)
type Loc = (Node, Cxt)

left :: Loc -> Loc
left (n, Descend dat leftNodes parentCxt rightNodes) = (last leftNodes, Descend dat (init leftNodes) parentCxt (n : rightNodes))
left (_, Top) = error "Tried to left on top"

right :: Loc -> Loc
right (n, Descend dat leftNodes parentCxt rightNodes) = (head rightNodes, Descend dat (leftNodes ++ [n]) parentCxt (tail rightNodes))
right (_, Top) = error "Tried to right on top"

down :: Int -> Loc -> Loc
down n (IntNode lhv rect children, cxt) = (children !! n, Descend (lhv, rect) (take n children) cxt (lastN (length children - n - 1) children))
down _ (LeafNode _, _) = error "Tried to down on leaf node"

up :: Loc -> Loc
up (_, Top) = error "Tried to go up on top"
up (n, Descend (lhv, rect) leftNodes cxt rightNodes) = (IntNode lhv rect (leftNodes ++ (n : rightNodes)), cxt)

treeTest = (IntNode 42 (Rect 0 0 10 10) [(LeafNode [
                                             (IDRect (Rect 0 0 10 10) "A1"),
                                             (IDRect (Rect 0 0 1 1) "A2"),
                                             (IDRect (Rect 2 2 7 7) "A3")
                                             ]),
                                         (LeafNode [
                                             (IDRect (Rect 0 0 10 10) "B1"),
                                             (IDRect (Rect 0 0 1 1) "B2")
                                             ]),
                                         (LeafNode [
                                             (IDRect (Rect 0 0 10 10) "C1"),
                                             (IDRect (Rect 0 0 1 1) "C2"),
                                             (IDRect (Rect 2 2 7 7) "C3")
                                             ])])

topLoc = (treeTest, Top)


-- chooseLeaf :: Rect -> Node -> Lens Node
