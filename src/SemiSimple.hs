module SemiSimple where

import Test.QuickCheck.Arbitrary
import Control.Monad

import Rational
import RootSystem
import Weyl
import CartanAlgebra

import Data.List(sort)

import qualified Data.Matrix as M

data Simple = A Int
            | B Int
            | C Int
            | D Int
            | G2 | F4 | E6 | E7 | E8 deriving(Eq,Ord,Show)

data SemiSimple = SemiSimple Int [Simple] deriving(Eq,Ord,Show)

fromSimples :: Int -> [Simple] -> SemiSimple
fromSimples torus simples = SemiSimple torus (sort simples)

product :: SemiSimple -> SemiSimple -> SemiSimple
product (SemiSimple torus1 simples1) (SemiSimple torus2 simples2) = fromSimples (torus1+torus2) (simples1++simples2)

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


rootSystemSimple :: Simple -> BasicRootSystem
rootSystemSimple (A n) = fromRoots $ map (BasicRoot . swapRoot (n+1)) [1..n]
    where swapRoot m i = M.setElem 1 (1,i) $ M.setElem 1 (-1,i+1) $ M.zero 1 m
rootSystemSimple (B n) = fromRoots $ negRoot : aRoots
    where (BasicRootSystem _ aRoots) = rootSystemSimple (A (n-1))
          negRoot = BasicRoot $ M.setElem 1 (1,1) $ M.zero 1 n
rootSystemSimple (C n) = fromRoots $ negLongRoot : aRoots
    where (BasicRootSystem _ aRoots) = rootSystemSimple (A (n-1))
          negLongRoot = BasicRoot $ M.setElem 2 (1,1) $ M.zero 1 n
rootSystemSimple (D n) = fromRoots $ negSwapRoot : aRoots
    where (BasicRootSystem _ aRoots) = rootSystemSimple (A (n-1))
          negSwapRoot = BasicRoot $ M.setElem 1 (1,1) $ M.setElem 1 (1,2) $ M.zero 1 n
rootSystemSimple G2 = fromRoots $ map (BasicRoot . M.fromList 1 3) $ [[0,1,-1] , [1,-2,1]]
rootSystemSimple F4 = fromRoots $ map (BasicRoot . M.fromList 1 4) $ [[0,1,-1,0],
                                                                      [0,0,1,-1],
                                                                      [0,0,0,1],
                                                                      [1/2,-1/2,-1/2,-1/2]]
rootSystemSimple E8 = fromRoots $ excepRoot : spin14Roots'
    where (BasicRootSystem _ spin14Roots) = rootSystemSimple (D 7)
          spin14Roots' = map (BasicRoot . M.extendTo 1 8 . coroot) spin14Roots
          excepRoot = BasicRoot $ M.fromList 1 8 $ replicate 8 (-1/2)
rootSystem E7 = fromRoots $ take 7 e8roots
    where (BasicRootSystem _ e8roots) = rootSystemSimple E8
rootSystem E6 = fromRoots $ take 6 e8roots
    where (BasicRootSystem _ e8roots) = rootSystemSimple E8

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