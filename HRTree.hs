module HRTree where

import Types
import Util
import Data.List (foldl', intercalate)

{- Params -}
cl = 10
cn = 10

{- Search -}
search :: Rect -> Node -> [ID]
search w (LeafNode idRects) = map getId $ filter ((rectanglesIntersect w) . getRect) idRects
search w (IntNode _ _ children) = concat $ map (search w) children

{- Insert -}
insert :: Rect -> Node -> [ID]
insert w tree = undefined


isFull :: Node -> Bool
isFull (LeafNode idRects) = length idRects == cl
isFull (IntNode _ _ children) = length children == cn

-- chooseLeaf :: Rect -> Node -> Lens Node
