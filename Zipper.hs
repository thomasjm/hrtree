module Zipper where

import Util
import Types

data Cxt node dat = Top | Descend dat [node] (Cxt node dat) [node] deriving (Show, Eq)
type Loc node dat = (node, Cxt node dat)

left :: Loc node dat -> Loc node dat
left (n, Descend dat leftNodes parentCxt rightNodes) = (last leftNodes, Descend dat (init leftNodes) parentCxt (n : rightNodes))
left (_, Top) = error "Tried to left on top"

right :: Loc node dat -> Loc node dat
right (n, Descend dat leftNodes parentCxt rightNodes) = (head rightNodes, Descend dat (leftNodes ++ [n]) parentCxt (tail rightNodes))
right (_, Top) = error "Tried to right on top"

replace :: node -> Loc node dat -> Loc node dat
replace n (_, cxt) = (n, cxt)

-- TODO: this could actually be generic if it weren't for "up"
root :: Loc Node (LHV, Rect) -> Node
root (n, Top) = n
root loc@(_, Descend _ _ _ _) = root (up loc)

down :: Int -> Loc Node (LHV, Rect) -> Loc Node (LHV, Rect)
down n (IntNode lhv rect children, cxt) = (children !! n, Descend (lhv, rect) (take n children) cxt (lastN (length children - n - 1) children))
down _ (LeafNode _, _) = error "Tried to down on leaf node"

up :: Loc Node (LHV, Rect) -> Loc Node (LHV, Rect)
up (_, Top) = error "Tried to go up on top"
up (n, Descend (lhv, rect) leftNodes cxt rightNodes) = (IntNode lhv rect (leftNodes ++ (n : rightNodes)), cxt)

makeZipper node = (node, Top)
