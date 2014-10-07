module HRTree where

import Types
import Util
import Zipper

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

treeTest = (IntNode 42 (Rect 0 0 10 10) [(LeafNode [
                                             (IDRect (Rect 0 0 10 10) "A1"),
                                             (IDRect (Rect 0 0 1 1) "A2"),
                                             (IDRect (Rect 2 2 7 7) "A3")
                                             ]),
                                         (LeafNode [
                                             (IDRect (Rect 0 0 10 10) "B1"),
                                             (IDRect (Rect 0 0 1 1) "B2")
                                             ]),
                                         (LeafNode [
                                             (IDRect (Rect 0 0 10 10) "C1"),
                                             (IDRect (Rect 0 0 1 1) "C2"),
                                             (IDRect (Rect 2 2 7 7) "C3")
                                             ])])

newLeaf = LeafNode [(IDRect (Rect 0 0 10 10) "D1"),
                    (IDRect (Rect 0 0 1 1) "D2"),
                    (IDRect (Rect 2 2 7 7) "D3")]

treeTop = makeZipper treeTest
