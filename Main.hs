module Main where

import Test.QuickCheck

import HRTree
import DiagramHRTree
import QuickcheckTests

import Diagrams.TwoD.Size
import Diagrams.Backend.SVG

mn = do
  randomTree <- generate tree
  let diagram = diagramHRTree randomTree
      filepath = "/tmp/diagram.svg"
  renderSVG filepath (mkSizeSpec Nothing Nothing) diagram
  putStrLn filepath
