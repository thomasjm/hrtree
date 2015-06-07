{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module QuickcheckTests where

import Test.QuickCheck

import HRTree
import Params
import Types
import Util

import Control.Monad
import Data.Function (on)
import Data.List (sortBy, sort)
import Data.Monoid

import Control.Exception
import Debug.Trace (trace)
import DiagramHRTree
import Shelly (run, liftIO, shelly, escaping)
import System.IO.Unsafe (unsafePerformIO)
import Zora.Graphing.DAGGraphing (render)


runTests = $quickCheckAll

--------------------------------------------------------------------------
-- * Arbitrary instances

instance Arbitrary Rect where
  arbitrary = do
    [xl, yl, dx, dy] <- vectorOf 4 arbitrary
    return $ Rect xl yl (xl + abs dx) (yl + abs dy)

instance Arbitrary IDRect where
  arbitrary = do
    r <- arbitrary
    id <- elements (['A'..'Z'] ++ ['a' .. 'z'])
    return $ IDRect r [id] $ hilbertDistanceFn $ rectCenter r

instance Arbitrary RTree where
  arbitrary = tree

tree :: Gen Node
tree = sized tree'
tree' :: Int -> Gen Node
tree' 0 = liftM LeafNode arbitrary
tree' n | n > 0 =
	oneof [liftM LeafNode (resize n $ listOf (arbitrary :: Gen IDRect)),
	       liftM3 IntNode arbitrary arbitrary (listOf (tree' (n `div` 2)))]

--------------------------------------------------------------------------
-- * Checking that a tree is actually valid

{- Check that a tree is valid. This includes
   1. The LHV and MBR of each interior node are correct
   2. The children of an interior node are arranged in increasing order of LHV -}
isValidTree :: Node -> Bool
isValidTree n@(IntNode lhv mbr children) = and [
  lhv == maximum (map getLHVRecursively children),
  mbr == mconcat (map getMBRRecursively children),
  prop_HilbertValsOrdered n
  ]
isValidTree (LeafNode _) = True

getLHVRecursively :: Node -> LHV
getLHVRecursively (IntNode lhv mbr children) = maximum $ map getLHVRecursively children
getLHVRecursively l@(LeafNode _) = getLHV l

getMBRRecursively :: Node -> Rect
getMBRRecursively (IntNode lhv mbr children) = mconcat $ map getMBRRecursively children
getMBRRecursively (LeafNode idRects) = mconcat (map getRect idRects)

prop_HilbertValsOrdered :: Node -> Bool
prop_HilbertValsOrdered (IntNode {}) = True -- Ignore interior nodes
prop_HilbertValsOrdered l@(LeafNode _) = isOrdered $ getHilbertValues l



--------------------------------------------------------------------------
-- * The actual properties



{- Inserting IDRects into a leaf puts them in in order of increasing Hilbert value -}
prop_orderedLeafInserts :: [IDRect] -> Bool
prop_orderedLeafInserts idRects = isOrdered $ getHilbertValues leafNode where
  leafNode = insertAll (LeafNode [])
  insertAll = foldl (.) id (map insert idRects)

prop_intersectionTests :: [IDRect] -> Rect -> Bool
prop_intersectionTests rects queryRect =
  sort (map getId intersectingRects) == sort (search queryRect builtTree)
  where
    intersectingRects = [x | x <- rects, (getRect x) `rectanglesIntersect` queryRect]
    builtTree = insertAll rects emptyRTree

prop_insertionMakesValidTree :: [IDRect] -> Bool
prop_insertionMakesValidTree idRects = isValidTree builtTree where
  builtTree = insertAll idRects emptyRTree

prop_generatedTreeIsValid :: RTree -> Bool
prop_generatedTreeIsValid = isValidTree
