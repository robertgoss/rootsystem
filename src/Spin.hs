{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Spin where

import Test.QuickCheck.Arbitrary
import Data.Matrix as M

import Rational
import RootSystem
import CartanAlgebra
import Permutation
import Signs
import Weyl

data SpinRoot = SwapRoot Int Int
                | SignSwapRoot Int Int
                | Neg SpinRoot deriving(Show)



newtype SpinSystem = SpinSystem Int deriving(Eq,Ord,Show)

data SpinWeylElement = SpinElement Signs Permutation deriving(Eq,Show,Ord)

data SpinWeyl = SpinWeyl Int deriving(Eq,Ord,Show)


makeSwapRoot pos neg | pos < neg = SwapRoot pos neg
                     | otherwise = Neg $ SwapRoot neg pos

makeSSwapRoot i j | i < j = SignSwapRoot i j
                  | otherwise = SignSwapRoot j i

makeNeg (Neg root) = root
makeNeg (root) = Neg root

spad n (SpinElement sign perm) = SpinElement (Signs.pad n sign) (Permutation.pad n perm)


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

    coroot (SwapRoot i j) = BasicRoot $ M.setElem 1 (1,i) $ M.setElem (-1) (1,j)  $ M.zero 1 j
    coroot (SignSwapRoot i j) = BasicRoot $ M.setElem 1 (1,i) $ M.setElem 1 (1,j)  $ M.zero 1 j
    coroot (Neg root) = RootSystem.negate $ coroot root

    positive (SwapRoot _ _) = True
    positive (SignSwapRoot _ _ ) = True
    positive (Neg root) = not $ positive root

    add root1 root2 | reflected == root1 = Nothing
                    | reflected == RootSystem.negate root1 = Nothing
                    | positive root2 && reflected < root1 = Nothing
                    | (not $ positive root2) && reflected > root1 = Nothing
                    | otherwise = Just reflected
        where reflected = root1 `reflect` root2

    negate (Neg root) = root
    negate root = Neg root

    lengthSq _ = 1

instance RootSystem SpinSystem SpinRoot where
    generators (SpinSystem 0) = []
    generators (SpinSystem 1) = []
    generators (SpinSystem n) = SignSwapRoot 1 2 : [SwapRoot i (i+1) | i<-[1..(n-1)]]
    rank (SpinSystem n) = n
    cartanAlgebra (SpinSystem n) = fullSubAlgebra n

instance WeylGroupElement SpinWeylElement SpinRoot where
    multiply (SpinElement sign1 perm1) (SpinElement sign2 perm2) = SpinElement sign perm
        where perm = perm2 `Permutation.combine` perm1
              sign = sign1 `Signs.combine` signPerm
              signPerm = permute perm1 sign2

    inverse (SpinElement sign perm) = SpinElement isign iperm
        where iperm = Permutation.inverse perm
              isign = permute iperm sign

    torusRepresentation (SpinElement sign perm) = BasicElement $ signMatrix * permMatrix
        where (Signs v) = sign
              (Perm w) = perm
              m = max (nrows v) (nrows w)
              signMatrix = Signs.toMatrix (Signs.pad m sign)
              permMatrix = Permutation.toMatrix (Permutation.pad m perm)

    simpleReflection (SwapRoot i j) = SpinElement sign perm
        where perm = swap i j $ Permutation.identity 1
              sign = Signs.identity (max i j)
    simpleReflection (SignSwapRoot i j) = SpinElement sign perm
        where perm = swap i j $ Permutation.identity 1
              sign = dSwap i j $ Signs.identity (max i j)
    simpleReflection (Neg root) = simpleReflection root

    weylAction (SpinElement sign perm) = spinSignAct sign . spinPermAct perm


spinSignAct :: Signs -> SpinRoot -> SpinRoot
spinSignAct sign (SwapRoot i j) | signI == 1 && signJ == 1 = SwapRoot i j
                                | signI == 1 && signJ == -1 = SignSwapRoot i j
                                | signI == -1 && signJ == 1 = Neg $ SignSwapRoot i j
                                | signI == -1 && signJ == -1 = Neg $ SwapRoot i j
     where signI = Signs.at i sign
           signJ = Signs.at j sign
spinSignAct sign (SignSwapRoot i j) | signI == 1 && signJ == 1 = SignSwapRoot i j
                                    | signI == 1 && signJ == -1 = SwapRoot i j
                                    | signI == -1 && signJ == 1 = Neg $ SwapRoot i j
                                    | signI == -1 && signJ == -1 = Neg $ SignSwapRoot i j
     where signI = Signs.at i sign
           signJ = Signs.at j sign
spinSignAct sign (Neg root) = RootSystem.negate $ spinSignAct sign root


spinPermAct :: Permutation -> SpinRoot -> SpinRoot
spinPermAct perm (SwapRoot i j) = makeSwapRoot (Permutation.at i perm) (Permutation.at j perm)
spinPermAct perm (SignSwapRoot i j) = makeSSwapRoot (Permutation.at i perm) (Permutation.at j perm)
spinPermAct perm (Neg root) = RootSystem.negate $ spinPermAct perm root


instance WeylGroup SpinWeyl SpinWeylElement SpinSystem SpinRoot where
   weylGroup (SpinSystem n) = SpinWeyl n

   one (SpinWeyl n) = SpinElement (Signs.identity n) (Permutation.identity n)

   generators (SpinWeyl n) = map simpleReflection $ RootSystem.generators $ SpinSystem n


instance Arbitrary SpinRoot where
    arbitrary = do i <- arbitrary
                   j <- arbitrary
                   neg <- arbitrary
                   sign <- arbitrary
                   let i' = (i `mod` 4) + 1
                       j' = (j `mod` 4) + 1
                       root = if sign then SwapRoot i' (i'+j') else SignSwapRoot i' (i'+j')
                   return $ if neg then Neg root else root

instance Arbitrary SpinSystem where
    arbitrary = do i <- arbitrary
                   return $ SpinSystem (i `mod` 5)

instance Arbitrary SpinWeylElement where
    arbitrary = do roots <- arbitrary
                   root <- arbitrary
                   let element = simpleReflection root
                       elements = map simpleReflection roots
                   return $ foldl multiply element elements

