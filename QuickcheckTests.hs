{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module QuickcheckTests where

import Test.QuickCheck

import HRTree
import Params
import Types
import Util

import Control.Monad
import Data.List (sortBy)
import Data.Function (on)

import Zora.Graphing.DAGGraphing (graph)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception

main = putStrLn "asdf"

instance Arbitrary RTree where
  arbitrary = tree

tree :: Gen Node
tree = sized tree'
tree' :: Int -> Gen Node
tree' 0 = liftM LeafNode arbitrary
tree' n | n>0 =
	oneof [liftM LeafNode shortIDRectList,
	       liftM3 IntNode arbitrary arbitrary shortNodeList]
  where subtree = tree' (n `div` 2)

{- Inserting IDRects into a leaf puts them in in order of increasing Hilbert value -}
prop_OrderedLeafInserts :: [IDRect] -> Bool
prop_OrderedLeafInserts idRects = isOrdered $ getHilbertValues leafNode where
  leafNode = insertAll (LeafNode [])
  insertAll = foldl (.) id (map insert idRects)

fullLeaf :: Gen Node
fullLeaf = liftM LeafNode $
           liftM (sortBy (compare `on` getHV)) $
           vectorOf cl (arbitrary :: Gen IDRect)

arbitraryLeafNode :: Gen Node
arbitraryLeafNode = liftM LeafNode $ shortIDRectList

shortNodeList :: Gen [Node]
shortNodeList = resize 3 $ listOf1 (arbitrary :: Gen Node)

shortIDRectList :: Gen [IDRect]
shortIDRectList = resize 5 $ listOf1 (arbitrary :: Gen IDRect)

randomIntNode :: Gen Node
randomIntNode = do
  children <- shortNodeList
  let lhv = maximum $ map getLHV children
      rect = boundingRect (map getBoundingRect children)
  return $ IntNode lhv rect $ sortBy (compare `on` getLHV) children

instance Arbitrary Rect where
  arbitrary = do
    [xl, yl, dx, dy] <- vectorOf 4 arbitrary
    return $ Rect xl yl (xl + abs dx) (yl + abs dy)

instance Arbitrary IDRect where
  arbitrary = do
    r <- arbitrary
    id <- elements (['A'..'Z'] ++ ['a' .. 'z'])
    return $ IDRect r [id] $ hilbertDistanceFn $ rectCenter r

test = insert newRect x where
    x = unsafePerformIO $ generate fullLeaf
    newRect = unsafePerformIO $ generate (arbitrary :: Gen IDRect)

randomInserts n = map insert $ unsafePerformIO $ generate $ vectorOf n (arbitrary :: Gen IDRect)

test2 n = f emptyRTree where
    randomInserts = map insert $ unsafePerformIO $ generate $ vectorOf n (arbitrary :: Gen IDRect)
    f = foldl (.) id randomInserts

{- Check that a tree is valid. This includes
   1. The LHV and MBR of each interior node are correct
   2. The children of an interior node are arranged in increasing order of LHV -}
isValidTree :: Node -> Bool
isValidTree n = undefined

dangerous :: a -> (a -> a) -> IO a
dangerous x f = do
  return (f x)

tryTransformations :: a -> [a -> a] -> a
tryTransformations x [] = x
tryTransformations x (f:fs) = case unsafePerformIO result of
                                Nothing -> x
                                Just newX -> tryTransformations newX fs
    where
      result = catch (dangerous (Just x) (liftM f)) (\e -> do
                                                       putStrLn $ "Got an error: " ++ (show $ (e :: SomeException))
                                                       return Nothing)
