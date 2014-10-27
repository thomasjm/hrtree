{-# LANGUAGE NoMonomorphismRestriction #-}
module DiagramHRTree where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

main = mainWith (circle 1 :: Diagram B R2)

mytest = mainWith (circle 1 :: Diagram B R2)
