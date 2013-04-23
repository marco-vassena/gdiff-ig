{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wall                   #-}

module Main where

import Generics.Instant.TH
import qualified Generics.Instant.Functions.Show as G
import Generics.Instant.GDiff
import Test.QuickCheck
import Test.QuickCheck.Gen
import System.Random
import Data.Typeable

-- Testing
data Tree = Leaf | Node Char Tree Tree deriving (Show, Eq, Typeable)
$(deriveAll ''Tree)

level :: Tree -> Int
level Leaf = 0
level (Node _ l r) = 1 + (max (level l) (level r))

instance G.Show   Tree where show'      = G.show
instance SEq      Tree where shallowEq  = shallowEqDef
instance Build    Tree where build      = buildDef
instance Children Tree where children   = childrenDef
instance GDiff Tree

instance Arbitrary Tree where
  arbitrary = sized genTree where
    genTree n | n <= 1    = elements [Leaf]
              | otherwise = do x <- elements "ABC"
                               p <- elements [n `div` 3 .. n - n `div` 3]
                               q <- elements [n `div` 3 .. n - n `div` 3]
                               l <- genTree (n-p)
                               r <- genTree (n-q)
                               return (Node x l r)

diffValid :: Tree -> Tree -> Property
diffValid s t = collect (max (level s) (level t)) $ 
                  patch (diff s t) s == Just t

testQuickCheck, main, testBig :: IO ()
testQuickCheck = quickCheck diffValid

main = quickCheck diffValid

--------------------------------------------------------------------------------
-- Performance testing

bigTree1, bigTree2 :: Tree
bigTree1 = unGen arbitrary (mkStdGen 123) 200
bigTree2 = unGen arbitrary (mkStdGen 124) 200

testBig = print $ diffLen bigTree1 bigTree2
