module SemiSimple where

import Test.QuickCheck.Arbitrary
import Control.Monad

import Rational
import RootSystem
import Weyl
import CartanAlgebra



import qualified Data.Matrix as M

data Simple = A Int
            | B Int
            | C Int
            | D Int
            | G2 | F4 | E6 | E7 | E8

data SemiSimple = SemiSimple Int [Simple]

rankSimple :: Simple -> Int
rankSimple (A n) = n*n + 2*n
rankSimple (B n) = n * (2*n + 1)
rankSimple (C n) = n * (2*n + 1)
rankSimple (D n) = n * (2*n - 1)
rankSimple G2 = 14
rankSimple F4 = 52
rankSimple E6 = 78
rankSimple E7 = 133
rankSimple E8 = 248




instance Arbitrary Simple where
    arbitrary = do n <- arbitrary
                   m <- liftM (`mod` 8) arbitrary
                   let types = [A m,B m,C m,D m,G2,F4,E6,E7,E8]
                   return $ types !! (n `mod` 9)

instance Arbitrary SemiSimple where
    arbitrary = do n <- arbitrary
                   m <- arbitrary
                   xs <- arbitrary
                   return $ SemiSimple (m `mod` 8) (take (n `mod` 3) xs)