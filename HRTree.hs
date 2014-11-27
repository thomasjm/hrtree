{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses #-}
module HRTree where

import Types
import Util
import Zipper

import Data.Function (on)
import Data.Maybe (fromJust, mapMaybe)
import Data.List (foldl', intercalate, find, findIndex, sortBy)

import System.Directory
import System.FilePath
import Text.Read

-- Debugging stuff
import Zora.Graphing.DAGGraphing (render)
import Debug.Trace (trace, traceShow)
import System.IO.Unsafe (unsafePerformIO)
import System.Random


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
insert idRect node = (trace ("Chosen leaf: " ++ (show $ focus leaf))) $
                     (trace ("Got new leaf: " ++ (show newLeaf))) $

                     leaf
                     -- & logState "chosen"
                     & replace newLeaf -- Insert the new leaf node

                     -- & logState "insert_done"

                     & handleOverflow -- Split the leaf node up if necessary
                     & root where
    f = hilbertDistanceOfRect . getRect

    leaf = chooseLeaf (f idRect) (makeZipper node)
    idRects = getIDRects $ focus leaf

    -- Make a new leaf node with the new IDRect inserted
    -- TODO: replace with more efficient sorted insert
    newLeaf = LeafNode ([x | x <- idRects, f x < f idRect] ++ [idRect] ++ [x | x <- idRects, f x >= f idRect])

newParentContaining :: [Node] -> Node
newParentContaining = fixupNode . (IntNode 0 (Rect 0 0 0 0))

fixupNode :: Node -> Node
fixupNode n@(LeafNode {}) = n -- Nothing to be done
fixupNode n@(IntNode _ _ children) = IntNode lhv rect children where
  lhv = maximum $ map getLHV children
  rect = boundingRect $ map getBoundingRect children

-- Handle overflow of leaf node. Takes in a NodeZipper that's pointed at a leaf node,
-- and splits it up if necessary. Returns the parent
handleOverflow :: NodeZipper -> NodeZipper
handleOverflow nz = unsafePerformIO $ do
                      -- logStateIO "before_handle_overflow" nz
                      let newTree = handleOverflow' nz
                      -- logStateIO "after_handle_overflow" newTree
                      return newTree


handleOverflow' :: NodeZipper -> NodeZipper
handleOverflow' nz
    -- If the node we added to is not full, just fix up its values and keep going up
    | not $ isOverflowed (focus nz) = if isTop nz
                                      then nz & replace (fixupNode (focus nz))
                                      else nz & replace (fixupNode (focus nz)) & up & handleOverflow
    -- If it is full but is at the top, return the new parent
    | isTop nz = makeZipper newParent
    -- If we're not at the top yet, recurse on the new parent
    | otherwise = up nz & replace newParent & handleOverflow

    where
      -- Try to distribute the nodes among the cooperating siblings.
      -- These siblings are of the same type as (focus nz). Thus, if we're trying
      -- to re-apportion IDRects, we'll only get the LeafNode siblings. Otherwise,
      -- if we're trying to divide up Nodes, we'll get the IntNode siblings.
      (siblings, others) = getCooperatingSiblings nz

      numNodesToDistributeAmong = if all isFullOrOverflowed siblings then length siblings + 1 else length siblings

      newSiblings :: [Node]
      newSiblings = case head siblings of
        (LeafNode {}) -> map LeafNode $ distributeItems numNodesToDistributeAmong (concatMap getIDRects siblings)
        (IntNode {}) -> map newParentContaining $ distributeItems numNodesToDistributeAmong (concatMap allChildren siblings)

      newParent = newParentContaining $ sortBy (compare `on` getLHV) (newSiblings ++ others)


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

----------------------------------------------------------------------------


logStateIO :: String -> NodeZipper -> IO NodeZipper
logStateIO s nz = do
  let num = unsafePerformIO (getStdRandom (randomR (0, 100000000))) :: Int
      filename = "/tmp/hrtree/" ++ s ++ "_" ++ (show num) ++ "_" ++ ".png"
  render filename $ root nz
  return nz

logState :: String -> NodeZipper -> NodeZipper
logState s nz = unsafePerformIO $ logStateIO s nz
