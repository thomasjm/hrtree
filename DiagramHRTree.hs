{-# LANGUAGE NoMonomorphismRestriction #-}
module DiagramHRTree where

import Types

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import System.Environment (withArgs)

r :: Diagram B R2 -> IO ()
r d = withArgs ["-o", "/tmp/hrtree.svg", "-w", "400"] $ mainWith (d # bg black)

rectAtOrigin w h = translate (r2 (w/2.0, h/2.0)) (rect w h)
rectTranslated w h x y = translate (r2 (x, y)) $ rectAtOrigin w h

fI = fromIntegral

diagramHRTree :: Node -> Diagram B R2
diagramHRTree node@(IntNode lhv (Rect xl yl xh yh) children) = diagramHRTree' (p2 (fI xl, fI yl)) node
diagramHRTree node@(LeafNode {}) = diagramHRTree' (p2 (0, 0)) node

diagramHRTree' :: P2 -> Node -> Diagram B R2
diagramHRTree' origin (IntNode lhv (Rect xl yl xh yh) children) =
    -- Draw the containing rectangle
    rectTranslated (fI xh - fI xl) (fI yh - fI yl) (fI xl) (fI yl) # lc red
    -- Draw a line from the origin to the containing rectangle
    <> fromVertices [origin, p2 (fI xl, fI xh)] # lc white
    -- Draw all the children
    <> (foldl1 (<>) (map (diagramHRTree' (p2 (fI xl, fI yl))) children))
diagramHRTree' origin (LeafNode idRects) = foldl1 (<>) (map drawIdRect idRects) where
    drawIdRect (IDRect (Rect xl yl xh yh) id hv) =
        (rectTranslated (fI xh - fI xl) (fI yh - fI yl) (fI xl) (fI yl) # lc blue)
        <> (fromVertices [origin, p2 (fI xl, fI yl)]) # lc white
