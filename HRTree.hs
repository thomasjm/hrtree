{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
module HRTree where

import Types
import Util
import Zipper

import Data.Function (on)
import Data.Maybe (fromJust)
import Data.List (foldl', intercalate, find, findIndex, sortBy)

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

    leaf = chooseLeaf (f idRect) (makeZipper node)
    idRects = getIDRects $ focus leaf

    -- Make a new leaf node with the new IDRect inserted
    -- TODO: replace with more efficient sorted insert
    newLeaf = LeafNode ([x | x <- idRects, f x < f idRect] ++ [idRect] ++ [x | x <- idRects, f x >= f idRect])

fixupNode :: Node -> Node
fixupNode n@(LeafNode {}) = n -- Nothing to be done
fixupNode n@(IntNode _ _ children) = IntNode lhv rect children where
  lhv = maximum $ map getLHV children
  rect = boundingRect $ map getBoundingRect children

-- Handle overflow of leaf node. Takes in a NodeZipper that's pointed at a leaf node,
-- and splits it up if necessary. Returns the parent
handleOverflow :: NodeZipper -> NodeZipper
handleOverflow nz
    -- If the node we added to is not full, just fix up its values and keep going up
    | not $ isOverflowed (focus nz) = if isTop nz
                                      then nz & replace (fixupNode (focus nz))
                                      else nz & replace (fixupNode (focus nz)) & up & handleOverflow
    -- If it is full but is at the top, return the new parent
    | isTop nz = makeZipper newParent
    -- If we're not at the top yet, recurse on the new parent
    | otherwise = nz & up & replace newParent & handleOverflow

    where
      -- Try to distribute the nodes among the cooperating siblings.
      -- These siblings are of the same type as (focus nz). Thus, if we're trying
      -- to re-apportion IDRects, we'll only get the LeafNode siblings. Otherwise,
      -- if we're trying to divide up Nodes, we'll get the IntNode siblings.
      (siblings, others) = getCooperatingSiblings nz

      numNodesToDistributeAmong = if all isFull siblings then length siblings + 1 else length siblings

      newSiblings :: [Node]
      newSiblings = case head siblings of
        (LeafNode {}) -> map LeafNode $ distributeItems numNodesToDistributeAmong (concatMap getIDRects siblings)
        (IntNode {}) -> map (IntNode 0 (Rect 0 0 0 0)) $ distributeItems numNodesToDistributeAmong (concatMap getIntChildren siblings)

      newParent = IntNode 0 (Rect 0 0 0 0) $ sortBy (compare `on` getLHV) (newSiblings ++ others)


-- fixParents :: NodeZipper -> NodeZipper
-- fixParents nz | isLeafNode $ focus nz = nz
--               | isTop nz = fixedNz
--               | otherwise = fixParents (up fixedNz) where
--         node@(IntNode _ _ children) = focus nz
--         fixedNz = replace newNode nz
--         newNode = IntNode (maximum $ map getLHV children) (boundingRect $ map getBoundingRect children) children


{- Get cooperating siblings for splitting purposes. Returns (chosen siblings, non-chosen siblings) -}
getCooperatingSiblings :: NodeZipper -> ([Node], [Node])
getCooperatingSiblings nz | isTop nz = ([focus nz], [])
                          | otherwise = case focus nz of
                                         (LeafNode _) -> (leafChildren, intChildren)
                                         (IntNode {}) -> (intChildren, leafChildren)
  where
    parent = focus (up nz)
    leafChildren = getLeafChildren parent
    intChildren = getIntChildren parent

emptyRTree = LeafNode []
