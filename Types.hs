{-# LANGUAGE DeriveDataTypeable #-}
module Types where

import Data.Typeable
import Data.Generics

import Data.List (intercalate)

data Node = IntNode { getIntLHV :: LHV, getIntRect :: Rect, allChildren :: [Node] } |
            LeafNode { getIDRects :: [IDRect] } deriving (Show, Eq, Data, Typeable)


type LHV = Integer
type ID = String

data Rect = Rect Integer Integer Integer Integer deriving (Eq, Data, Typeable)
instance Show Rect where
  -- show (Rect xl yl xh yh) = "R"
  show (Rect xl yl xh yh) = "(" ++ (intercalate ", " $ map show [xl, yl, xh, yh]) ++ ")"

data IDRect = IDRect {
  getRect :: Rect,
  getId :: ID,
  getHV :: Integer
  } deriving (Eq, Data, Typeable)

instance Show IDRect where
  -- show (IDRect rect id) = "(" ++ id ++ ", " ++ (show rect) ++ ")"
  show (IDRect _ id hv) = "(" ++ id ++ "," ++ show hv ++ ")"



type RTree = Node
