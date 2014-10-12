{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
module HRTree where

import Types
import Util
import Zipper

import Data.Maybe (fromJust)
import Data.List (foldl', intercalate, find, findIndex)
import Data.List.Split (chunksOf)

import Debug.Trace (trace)

{- Define a TreeZippable instance for Node and a convenience type synonym -}
instance TreeZippable Node (LHV, Rect) where
  getData (IntNode lhv mbr _) = (lhv, mbr)
  getChildren (IntNode _ _ children) = children
  newNode (lhv, mbr) = IntNode lhv mbr

type NodeZipper = Loc Node (LHV, Rect)

{- Search -}
search :: Rect -> Node -> [ID]
search w (LeafNode idRects) = map getId $ filter (rectanglesIntersect w . getRect) idRects
search w (IntNode _ _ children) = concatMap (search w) children

chooseLeaf :: Integer -> NodeZipper -> NodeZipper
chooseLeaf h n = case focus n of
  (LeafNode _) -> n
  (IntNode _ _ children) -> case findIndex (\x -> getLHV x > h) children of
                             -- Choose the first child with an LHV greater than h
                             Just i -> chooseLeaf h (down i n)
                             -- Choose the rightmost child
                             Nothing -> chooseLeaf h (down (length children - 1) n)

{- Insert -}
insert :: IDRect -> RTree -> RTree
insert idRect node = replace newLeaf leaf -- Insert the new leaf node
                     & handleOverflow -- Split the leaf node up if necessary
                     & root where
    f = hilbertDistanceOfRect . getRect

    newHilbertVal = f idRect

    leaf = chooseLeaf newHilbertVal (makeZipper node)
    idRects = getIDRects $ focus leaf
    newLeaf = LeafNode ([x | x <- idRects, f x < newHilbertVal] ++ [idRect] ++ [x | x <- idRects, f x >= newHilbertVal])

-- Handle overflow of leaf node. Takes in a NodeZipper that's pointed at a leaf node,
-- and splits it up if necessary. Returns the parent
handleOverflow :: NodeZipper -> NodeZipper
handleOverflow nz
    -- If the node we added to is not full, just fix up its parents
    | not $ isFull (focus nz) = fixParents nz
    -- If it is full but is at the top, return the new parent
    | isTop nz = makeZipper newParent
    -- If we're not at the top yet, recurse on the new parent
    | otherwise = nz & up & replace newParent & handleOverflow

    where
      siblings = getSiblingNodes nz

      epsilon = concatMap getIDRects siblings
      maxLHV = maximum $ map getHV epsilon
      mbr = boundingRect $ map getRect epsilon

      numNodesToDistributeAmong = if all isFull siblings then length siblings + 1 else length siblings
      newSiblings = distributeRects numNodesToDistributeAmong epsilon
      newParent = IntNode maxLHV mbr newSiblings


fixParents :: NodeZipper -> NodeZipper
fixParents nz | isLeafNode $ focus nz = nz
              | isTop nz = fixedNz
              | otherwise = fixParents (up fixedNz) where
        node@(IntNode _ _ children) = focus nz
        fixedNz = replace newNode nz
        newNode = IntNode (maximum $ map getLHV children) (boundingRect $ map getBoundingRect children) children




-- Break the list of IDRects into n LeafNodes
distributeRects :: Int -> [IDRect] -> [Node]
distributeRects n idRects = map LeafNode $ chunksOf chunkSize idRects where
  chunkSize = let (quot, rem) = length idRects `quotRem` n in
    quot + (if rem == 0 then 0 else 1)

getSiblingNodes :: NodeZipper -> [Node]
getSiblingNodes nz = if isTop nz then [focus nz] else getIntChildren $ focus (up nz)

emptyRTree = LeafNode []
