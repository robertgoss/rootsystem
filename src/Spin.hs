{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Spin where

import Data.Permute
import Test.QuickCheck.Arbitrary
import Data.Matrix as M

import RootSystem
import CartanAlgebra

data SpinRoot = SwapRoot Int Int
                | SignSwapRoot Int Int
                | Neg SpinRoot deriving(Show)

instance Eq SpinRoot where
    (SwapRoot i j) == (SwapRoot m n) = i==m && j==n
    (SignSwapRoot i j) == (SignSwapRoot m n) = i==m && j==n
    (Neg root1) == (Neg root2) = root1 == root2
    root1 == (Neg root2) = False
    (SwapRoot _ _) == (SignSwapRoot _ _) = False
    root1 == root2 = root2 == root1

instance Ord SpinRoot where
    (SwapRoot i j) `compare` (SwapRoot m n) | i/=m = m `compare` i
                                            | otherwise = j `compare` n
    (SwapRoot i j) `compare` (SignSwapRoot m n) | i/=m = m `compare` i
                                                | otherwise = LT
    (SignSwapRoot i j) `compare` (SignSwapRoot m n) | i/=m = m `compare` i
                                                    | otherwise = n `compare` j
    (Neg root1) `compare` (Neg root2) = case root1 `compare` root2 of
                                          EQ -> EQ
                                          LT -> GT
                                          GT -> LT
    root1 `compare` (Neg root2) = GT
    root1 `compare` root2 = case root2 `compare` root1 of
                                EQ -> EQ
                                LT -> GT
                                GT -> LT


newtype SpinSystem = SpinSystem Int deriving(Eq,Ord,Show)

makeSwapRoot pos neg | pos < neg = SwapRoot pos neg
                     | otherwise = Neg $ SwapRoot neg pos

makeSSwapRoot i j | i < j = SignSwapRoot i j
                  | otherwise = SignSwapRoot j i

makeNeg (Neg root) = root
makeNeg (root) = Neg root

instance Root SpinRoot where
    reflect (SwapRoot i j) (SwapRoot m n)
                | i == m && j == n = makeNeg $ SwapRoot i j
                | i == m = makeSwapRoot n j
                | i == n = makeSwapRoot m j
                | j == m = makeSwapRoot i n
                | j == n = makeSwapRoot i m
                | otherwise  = SwapRoot i j
    reflect (SwapRoot i j) (SignSwapRoot m n)
                | i == m && j == n = SwapRoot i j
                | i == m = makeNeg $ makeSSwapRoot n j
                | i == n = makeNeg $ makeSSwapRoot m j
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
                | i == m && j == n = makeNeg $ SignSwapRoot i j
                | i == m = makeSwapRoot j n
                | i == n = makeSwapRoot j m
                | j == m = makeSwapRoot i n
                | j == n = makeSwapRoot i m
                | otherwise = SignSwapRoot i j
    reflect (Neg root1) root2 = makeNeg $ reflect root1 root2
    reflect root1 (Neg root2) = reflect root1 root2

    coroot (SwapRoot i j) = M.setElem 1 (1,i) $ M.setElem (-1) (1,j)  $ M.zero 1 j
    coroot (SignSwapRoot i j) = M.setElem 1 (1,i) $ M.setElem 1 (1,j)  $ M.zero 1 j
    coroot (Neg root) = scaleMatrix (-1) $ coroot root

    positive (SwapRoot _ _) = True
    positive (SignSwapRoot _ _ ) = True
    positive (Neg root) = not $ positive root

    add (SwapRoot i j) (SwapRoot m n) | j==m && i/=n = Just $ makeSwapRoot i n
                                      | i==n && j/=m = Just $ makeSwapRoot m j
                                      | otherwise = Nothing
    add (SwapRoot i j) (Neg (SwapRoot m n))
                                      | j==n && i/=m = Just $ makeSwapRoot i m
                                      | i==m && j/=n = Just $ makeSwapRoot n j
                                      | otherwise = Nothing
    add (SwapRoot i j) (SignSwapRoot m n ) | j==m && i/=n = Just $ makeSSwapRoot i n
                                           | j==n && i/=m = Just $ makeSSwapRoot i m
                                           | otherwise = Nothing
    add (SwapRoot i j) (Neg (SignSwapRoot m n))
                                           | i==m && j/=n = Just $ Neg $ makeSSwapRoot j n
                                           | i==n && j/=m = Just $ Neg $ makeSSwapRoot j m
                                           | otherwise = Nothing
    add (Neg (SwapRoot i j)) (SignSwapRoot m n)
                                           | i==m && j/=n = Just $ makeSSwapRoot j n
                                           | i==n && j/=m = Just $ makeSSwapRoot j m
                                           | otherwise = Nothing
    add (SignSwapRoot _ _) (SignSwapRoot _ _) = Nothing
    add (SignSwapRoot _ _) (Neg (SignSwapRoot _ _)) = Nothing
    add (Neg root1) (Neg root2) = fmap Neg $ add root1 root2
    add root1 root2 = add root2 root1

instance RootSystem SpinSystem SpinRoot where
    generators (SpinSystem 0) = []
    generators (SpinSystem 1) = []
    generators (SpinSystem n) = SignSwapRoot 1 2 : [SwapRoot i (i+1) | i<-[1..(n-1)]]
    rank (SpinSystem n) = n
    cartanAlgebra (SpinSystem n) = fullSubAlgebra n



instance Arbitrary SpinRoot where
    arbitrary = do i <- arbitrary
                   j <- arbitrary
                   neg <- arbitrary
                   sign <- arbitrary
                   let i' = (i `mod` 5) + 1
                       j' = (j `mod` 5) + 1
                       root = if sign then SwapRoot i' (i'+j') else SignSwapRoot i' (i'+j')
                   return $ if neg then Neg root else root

instance Arbitrary SpinSystem where
    arbitrary = do i <- arbitrary
                   return $ SpinSystem (i `mod` 5)
