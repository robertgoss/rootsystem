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

dimSimple :: Simple -> Int
dimSimple (A n) = n*n + 2*n
dimSimple (B n) = n * (2*n + 1)
dimSimple (C n) = n * (2*n + 1)
dimSimple (D n) = n * (2*n - 1)
dimSimple G2 = 14
dimSimple F4 = 52
dimSimple E6 = 78
dimSimple E7 = 133
dimSimple E8 = 248

dim :: SemiSimple -> Int
dim (SemiSimple torus simples) = torus + sum (map dimSimple simples)

rankSimple :: Simple -> Int
rankSimple (A n) = n
rankSimple (B n) = n
rankSimple (C n) = n
rankSimple (D n) = n
rankSimple G2 = 2
rankSimple F4 = 4
rankSimple E6 = 6
rankSimple E7 = 7
rankSimple E8 = 8

rank :: SemiSimple -> Int
rank (SemiSimple torus simples) = torus + sum (map rankSimple simples)




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