module Types where


import Data.List (intercalate)

type LHV = Integer
type ID = String
data Rect = Rect Integer Integer Integer Integer deriving (Eq)
instance Show Rect where
  show (Rect xl yl xh yh) = "(" ++ (intercalate ", " $ map show [xl, yl, xh, yh]) ++ ")"
data IDRect = IDRect { getRect :: Rect, getId :: ID } deriving (Eq)
instance Show IDRect where
  show (IDRect rect id) = "(" ++ id ++ ", " ++ (show rect) ++ ")"
data Node = IntNode LHV Rect [Node] | LeafNode LHV [IDRect] deriving (Show, Eq)

type RTree = Node
