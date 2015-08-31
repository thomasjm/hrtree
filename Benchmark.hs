module Main where

import Test.QuickCheck

import DiagramHRTree
-- import HRTree
import QuickcheckTests

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import Criterion.Main

-- main = do
--   randomTree <- generate tree
--   let diagram = diagramHRTree randomTree
--       filepath = "/tmp/diagram.svg"
--   renderSVG filepath (mkSizeSpec Nothing Nothing) diagram
--   putStrLn filepath
