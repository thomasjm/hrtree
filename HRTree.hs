{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
module HRTree where

import Types
import Util
import Zipper

import Data.Maybe (fromJust)
import Data.List (foldl', intercalate, find, findIndex)



{- Params -}
cl = 10
cn = 10

{- Search -}
search :: Rect -> Node -> [ID]
search w (LeafNode idRects) = map getId $ filter (rectanglesIntersect w . getRect) idRects
search w (IntNode _ _ children) = concat $ map (search w) children

{- Insert -}
insert :: Rect -> Node -> [ID]
insert w tree = undefined


isFull :: Node -> Bool
isFull (LeafNode idRects) = length idRects == cl
isFull (IntNode _ _ children) = length children == cn

chooseLeaf :: Integer -> NodeZipper -> NodeZipper
chooseLeaf h n = case focus n of
  (LeafNode _) -> n
  (IntNode _ _ children) -> case findIndex (\x -> getLHV x > h) children of
                             -- Choose the first child with an LHV greater than h
                             Just i -> chooseLeaf h (down i n)
                             -- Choose the rightmost child
                             Nothing -> chooseLeaf h (down (length children - 1) n)

treeTest :: RTree
treeTest = (IntNode 136 (Rect 0 0 10 10) [(LeafNode [
                                             (IDRect (Rect 0 0 10 10) "A1"),
                                             (IDRect (Rect 0 0 1 1) "A2"),
                                             (IDRect (Rect 2 2 7 7) "A3")
                                             ]),
                                         (LeafNode [
                                             (IDRect (Rect 0 0 10 10) "B1"),
                                             (IDRect (Rect 0 0 1 1) "B2")
                                             ]),
                                         (LeafNode [
                                             (IDRect (Rect 0 0 1 1) "C1"),
                                             (IDRect (Rect 2 2 7 7) "C2")
                                             ])])

newLeaf = LeafNode [(IDRect (Rect 0 0 10 10) "D1"),
                    (IDRect (Rect 0 0 1 1) "D2"),
                    (IDRect (Rect 2 2 7 7) "D3")]

treeTop = makeZipper treeTest

type NodeZipper = Loc Node (LHV, Rect)

instance TreeZippable Node (LHV, Rect) where
  getData (IntNode lhv mbr _) = (lhv, mbr)
  getChildren (IntNode _ _ children) = children
  newNode (lhv, mbr) children = IntNode lhv mbr children
