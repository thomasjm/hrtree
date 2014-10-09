module QuickcheckTests where

import Test.QuickCheck

import Types
import Control.Monad

main = putStrLn "asdf"

instance Arbitrary Node where
  arbitrary = do
    x <- tree
    return x

tree :: Gen Node
tree = sized tree'

tree' :: Int -> Gen Node
tree' 0 = liftM2 LeafNode arbitrary arbitrary
tree' n | n>0 =
	oneof [liftM2 LeafNode arbitrary arbitrary,
	       liftM3 IntNode arbitrary arbitrary arbitrary]
  where subtree = tree' (n `div` 2)


-- instance Arbitrary Char where
--   arbitrary = elements (['A'..'Z'] ++ ['a' .. 'z'])

instance Arbitrary Rect where
  arbitrary = do
    xl <- arbitrary
    yl <- arbitrary
    dx <- arbitrary
    dy <- arbitrary
    return $ Rect xl yl (xl + abs dx) (yl + abs dy)

instance Arbitrary IDRect where
  arbitrary = do
    r <- arbitrary
    id <- elements (['A'..'Z'] ++ ['a' .. 'z'])
    return $ IDRect r [id]
