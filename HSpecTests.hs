module Main where

import HRTree (rectanglesIntersect, Rect(Rect))
import Test.Hspec

main = hspec $ do
         describe "Rectangle intersections" $ do
                 it "should identify when two rectangles don't intersect" $
                   rectanglesIntersect (Rect 0 0 2 2) (Rect 3 3 4 4) `shouldBe` False
                 it "should identify when two rectangles do intersect" $ do
                   rectanglesIntersect (Rect 0 0 2 2) (Rect 1 1 3 3) `shouldBe` True
                   rectanglesIntersect (Rect 0 0 2 2) (Rect 0 0 1 1) `shouldBe` True
                   rectanglesIntersect (Rect 0 0 2 2) (Rect 0 1 1 2) `shouldBe` True
                 it "should work when one rectangle is a sliver" $ do
                   rectanglesIntersect (Rect 0 0 2 2) (Rect 0 1 1 1) `shouldBe` True
