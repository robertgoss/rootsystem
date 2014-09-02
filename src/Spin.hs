module Spin where

import Data.Permute
import Test.QuickCheck.Arbitrary
import Data.Matrix as M

import RootSystem

data SpinRoot = SwapRoot Int Int
                | SignSwapRoot Int Int
                | Neg SpinRoot

makeSwapRoot i j | i < j = SwapRoot i j
                 | otherwise = Neg $ SwapRoot i j

makeSSwapRoot i j | i < j = SignSwapRoot i j
                  | otherwise = Neg $ SignSwapRoot i j

instance Root SpinRoot where
    reflect (SwapRoot i j) (SwapRoot m n)
                | i == m && j == n = Neg $ SwapRoot i j
                | i == m = makeSwapRoot n j
                | i == n = makeSwapRoot m j
                | j == m = makeSwapRoot i n
                | j == n = makeSwapRoot i m
                | otherwise  = SwapRoot i j
    reflect (SwapRoot i j) (SignSwapRoot m n)
                | i == m && j == n = SwapRoot i j
                | i == m = makeSSwapRoot n j
                | i == n = makeSSwapRoot m j
                | j == m = makeSSwapRoot i n
                | j == n = makeSSwapRoot i m
                | otherwise = SwapRoot i j
    reflect (SignSwapRoot i j) (SwapRoot m n)
                | i == m && j == n = SignSwapRoot i j
                | i == m = makeSSwapRoot j n
                | i == n = makeSSwapRoot j m
                | j == m = makeSSwapRoot i n
                | j == n = makeSSwapRoot i m
                | otherwise = SignSwapRoot i j
    reflect (SignSwapRoot i j) (SignSwapRoot m n)
                | i == m && j == n = Neg $ SignSwapRoot i j
                | i == m = makeSwapRoot j n
                | i == n = makeSwapRoot j m
                | j == m = makeSwapRoot i n
                | j == n = makeSwapRoot i m
                | otherwise = SignSwapRoot i j
    reflect (Neg root1) root2 = Neg $ reflect root1 root2
    reflect root1 (Neg root2) = Neg $ reflect root1 root2

    coroot (SwapRoot i j) = M.setElem 1 (1,i) $ M.setElem (-1) (1,j)  $ M.zero 1 j
    coroot (SignSwapRoot i j) = M.setElem 1 (1,i) $ M.setElem 1 (1,j)  $ M.zero 1 j
    coroot (Neg root) = scaleMatrix (-1) $ coroot root

    positive (SwapRoot _ _) = True
    positive (SignSwapRoot _ _ ) = True
    positive (Neg root) = not $ positive root

    add _ _ = undefined




instance Arbitrary SpinRoot where
    arbitrary = do i <- arbitrary
                   j <- arbitrary
                   neg <- arbitrary
                   sign <- arbitrary
                   let i' = i `mod` 5
                       j' = j `mod` 5
                       root = if sign then SwapRoot i' (i'+j') else SignSwapRoot i' (i'+j')
                   return $ if neg then Neg root else root