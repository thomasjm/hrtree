{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Zipper (makeZipper, up, down, left, right, replace, root, focus, Loc, TreeZippable(..)) where

import Util

data Cxt node dat = Top | Descend dat [node] (Cxt node dat) [node] deriving (Show, Eq)
data Loc node dat = Loc (node, Cxt node dat) deriving (Show, Eq)

class TreeZippable node dat | node -> dat where
  getData :: node -> dat
  getChildren :: node -> [node]
  newNode :: dat -> [node] -> node

left :: Loc node dat -> Loc node dat
left (Loc (n, Descend dat leftNodes parentCxt rightNodes)) = Loc (last leftNodes, Descend dat (init leftNodes) parentCxt (n : rightNodes))
left (Loc (_, Top)) = error "Tried to left on top"

right :: Loc node dat -> Loc node dat
right (Loc (n, Descend dat leftNodes parentCxt rightNodes)) = Loc (head rightNodes, Descend dat (leftNodes ++ [n]) parentCxt (tail rightNodes))
right (Loc (_, Top)) = error "Tried to right on top"

replace :: node -> Loc node dat -> Loc node dat
replace n (Loc (_, cxt)) = Loc (n, cxt)

root :: (TreeZippable node dat) => Loc node dat -> node
root (Loc (n, Top)) = n
root loc@(Loc (_, Descend _ _ _ _)) = root (up loc)

down :: (TreeZippable node dat) => Int -> Loc node dat -> Loc node dat
down n (Loc (node, cxt)) = Loc (children !! n, Descend dat (take n children) cxt (lastN (length children - n - 1) children)) where
  children = getChildren node
  dat = getData node

up :: (TreeZippable node dat) => Loc node dat -> Loc node dat
up (Loc (_, Top)) = error "Tried to go up on top"
up (Loc (n, Descend dat leftNodes cxt rightNodes)) = Loc (newNode dat (leftNodes ++ (n : rightNodes)), cxt)

focus :: Loc node dat -> node
focus (Loc (node, _)) = node

makeZipper :: node -> Loc node dat
makeZipper node = Loc (node, Top)
