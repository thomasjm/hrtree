{-# LANGUAGE TypeSynonymInstances #-}
module QuickcheckTests where

import Test.QuickCheck

import HRTree
import Util
import Types
import Control.Monad

import Data.List (sortBy)
import Data.Function (on)

import Zora.Graphing.DAGGraphing

instance DAGGraphable Node where
  expand (LeafNode idRects) = Just (Just $ show idRects, [])
  expand (IntNode lhv rect children) = Just (Just $ show (lhv, rect), map f children) where
    f n = (Just "", n)

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


shortNodeList :: Gen [Node]
shortNodeList = resize 3 $ listOf1 (arbitrary :: Gen Node)

shortIDRectList :: Gen [IDRect]
shortIDRectList = resize 2 $ listOf1 (arbitrary :: Gen IDRect)

randomIntNode :: Gen Node
randomIntNode = do
  children <- shortNodeList
  let lhv = maximum $ map getLHV children
      rect = boundingRect (map getBoundingRect children)
  return $ IntNode lhv rect $ sortBy (compare `on` getLHV) children

instance Arbitrary Rect where
  arbitrary = do
    xl <- arbitrary
    yl <- arbitrary
    dx <- arbitrary
    dy <- arbitrary
    return $ Rect xl yl (xl + abs dx) (yl + abs dy)

instance Arbitrary IDRect where
  arbitrary = do
    r <- arbitrary
    id <- elements (['A'..'Z'] ++ ['a' .. 'z'])
    return $ IDRect r [id]
