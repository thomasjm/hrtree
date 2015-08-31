module Main where

import Test.QuickCheck

import DiagramHRTree
import HRTree
import QuickcheckTests
import Types

import Diagrams.Backend.SVG
import Diagrams.TwoD.Size

import Criterion.Main

-- main = do
--   randomTree <- generate tree
--   let diagram = diagramHRTree randomTree
--       filepath = "/tmp/diagram.svg"
--   renderSVG filepath (mkSizeSpec Nothing Nothing) diagram
--   putStrLn filepath


insertAndQuery :: Int -> IO Bool
insertAndQuery n = do
  idRects <- generate $ vectorOf n arbitrary
  let tree = insertAll idRects emptyRTree
  return $ ((length $ concat [search (getRect r) tree | r <- idRects]) == n)


main = defaultMain [
  bgroup "insert_and_query" [
      bench "10"  $ nfIO (insertAndQuery 10)
      , bench "100"  $ nfIO (insertAndQuery 100)
      ]
  ]
