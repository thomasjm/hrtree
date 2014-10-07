module Zipper (makeZipper, up, down, left, right, replace, root, focus) where

import Util
import Types

data Cxt node dat = Top | Descend dat [node] (Cxt node dat) [node] deriving (Show, Eq)
data Loc node dat = Loc (node, Cxt node dat) deriving (Show, Eq)

left :: Loc node dat -> Loc node dat
left (Loc (n, Descend dat leftNodes parentCxt rightNodes)) = Loc (last leftNodes, Descend dat (init leftNodes) parentCxt (n : rightNodes))
left (Loc (_, Top)) = error "Tried to left on top"

right :: Loc node dat -> Loc node dat
right (Loc (n, Descend dat leftNodes parentCxt rightNodes)) = Loc (head rightNodes, Descend dat (leftNodes ++ [n]) parentCxt (tail rightNodes))
right (Loc (_, Top)) = error "Tried to right on top"

replace :: node -> Loc node dat -> Loc node dat
replace n (Loc (_, cxt)) = Loc (n, cxt)

-- TODO: this could actually be generic if it weren't for "up"
root :: Loc Node (LHV, Rect) -> Node
root (Loc (n, Top)) = n
root loc@(Loc (_, Descend _ _ _ _)) = root (up loc)

down :: Int -> Loc Node (LHV, Rect) -> Loc Node (LHV, Rect)
down n (Loc (IntNode lhv rect children, cxt)) = Loc (children !! n, Descend (lhv, rect) (take n children) cxt (lastN (length children - n - 1) children))
down _ (Loc (LeafNode _, _)) = error "Tried to down on leaf node"

up :: Loc Node (LHV, Rect) -> Loc Node (LHV, Rect)
up (Loc (_, Top)) = error "Tried to go up on top"
up (Loc (n, Descend (lhv, rect) leftNodes cxt rightNodes)) = Loc (IntNode lhv rect (leftNodes ++ (n : rightNodes)), cxt)

focus :: Loc node dat -> node
focus (Loc (node, _)) = node

makeZipper :: node -> Loc node dat
makeZipper node = Loc (node, Top)
