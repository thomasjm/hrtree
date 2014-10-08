 {-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module HRTree where

import Types
import Util
import Zipper

import Data.Maybe (fromJust)
import Data.List (foldl', intercalate, find)

{- Params -}
cl = 10
cn = 10

{- Search -}
search :: Rect -> Node -> [ID]
search w (LeafNode _ idRects) = map getId $ filter ((rectanglesIntersect w) . getRect) idRects
search w (IntNode _ _ children) = concat $ map (search w) children

{- Insert -}
insert :: Rect -> Node -> [ID]
insert w tree = undefined


isFull :: Node -> Bool
isFull (LeafNode _ idRects) = length idRects == cl
isFull (IntNode _ _ children) = length children == cn

chooseLeaf :: Integer -> Node -> Loc Node (LHV, Rect)
chooseLeaf _ n@(LeafNode _ _) = makeZipper n
chooseLeaf h (IntNode _ _ children) = makeZipper $ fromJust $ find (\x -> (getLHV x) > h) children

hilbertDistanceFn :: (Integer, Integer) -> Integer
hilbertDistanceFn = hilbertDistance (10 :: Int)

getLHV :: Node -> Integer
getLHV (IntNode lhv _ _) = lhv
getLHV (LeafNode lhv _) = lhv

treeTest :: RTree
treeTest = (IntNode 42 (Rect 0 0 10 10) [(LeafNode 10 [
                                             (IDRect (Rect 0 0 10 10) "A1"),
                                             (IDRect (Rect 0 0 1 1) "A2"),
                                             (IDRect (Rect 2 2 7 7) "A3")
                                             ]),
                                         (LeafNode 20 [
                                             (IDRect (Rect 0 0 10 10) "B1"),
                                             (IDRect (Rect 0 0 1 1) "B2")
                                             ]),
                                         (LeafNode 30 [
                                             (IDRect (Rect 0 0 10 10) "C1"),
                                             (IDRect (Rect 0 0 1 1) "C2"),
                                             (IDRect (Rect 2 2 7 7) "C3")
                                             ])])

newLeaf = LeafNode 15 [(IDRect (Rect 0 0 10 10) "D1"),
                    (IDRect (Rect 0 0 1 1) "D2"),
                    (IDRect (Rect 2 2 7 7) "D3")]

treeTop = makeZipper treeTest

instance TreeZippable Node (LHV, Rect) where
  getData (IntNode lhv mbr _) = (lhv, mbr)
  getChildren (IntNode _ _ children) = children
  newNode (lhv, mbr) children = IntNode lhv mbr children
