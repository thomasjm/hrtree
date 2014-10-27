module Main where

import HRTree
import Util
import Types

import Test.Hspec

main = hspec $ do
  intersectionTest
  searchTest

intersectionTest =  describe "Rectangle intersections" $ do
  it "should identify when two rectangles don't intersect" $
    rectanglesIntersect (Rect 0 0 2 2) (Rect 3 3 4 4) `shouldBe` False
  it "should identify when two rectangles do intersect" $ do
    rectanglesIntersect (Rect 0 0 2 2) (Rect 1 1 3 3) `shouldBe` True
    rectanglesIntersect (Rect 0 0 2 2) (Rect 0 0 1 1) `shouldBe` True
    rectanglesIntersect (Rect 0 0 2 2) (Rect 0 1 1 2) `shouldBe` True
  it "should work when one rectangle is a sliver" $ do
    rectanglesIntersect (Rect 0 0 2 2) (Rect 0 1 1 1) `shouldBe` True

searchTest = describe "Search" $ do
  it "should work on a single leaf node" $
    search (Rect 3 3 5 5) (LeafNode [
                              (IDRect (Rect 0 0 10 10) "rect1" 0),
                              (IDRect (Rect 0 0 1 1) "rect2" 0),
                              (IDRect (Rect 2 2 7 7) "rect3" 0)
                              ])
    `shouldBe` ["rect1", "rect3"]

  it "should work on a single interior node with a single leaf node" $
    search (Rect 3 3 5 5) (IntNode 42 (Rect 0 0 10 10)
                           [(LeafNode [
                               (IDRect (Rect 0 0 10 10) "rect1" 0),
                               (IDRect (Rect 0 0 1 1) "rect2" 0),
                               (IDRect (Rect 2 2 7 7) "rect3" 0)
                           ])])
    `shouldBe` ["rect1", "rect3"]

-- Local Variables:
-- compile-command: "cabal test"
-- End:
