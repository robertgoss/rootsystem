{-# LANGUAGE MultiParamTypeClasses #-}
module E8 where

import Data.Matrix
import Test.QuickCheck.Arbitrary

import RootSystem
import Weyl
import Signs
import Spin
import Rational
import CartanAlgebra

type Signs0 = Signs
type Signs2 = Signs
type Signs4 = Signs
type Signs6 = Signs
type Signs8 = Signs

signType :: Signs -> Int
signType (Signs v) = length $ filter (==(-1)) $ toList v

neg :: Signs -> Signs
neg (Signs v) = Signs $ scaleMatrix (-1) v

data E8Root = E8SpinRoot SpinRoot
              | E8SRoot Signs deriving(Show)


data E8System = E8System


data E8WeylElement = E8Type1 SpinWeylElement
                     | E8Type2 Signs SpinWeylElement
                     | E8Type3 Signs SpinWeylElement
                     | E8Type3' Signs SpinWeylElement

data E8Weyl = E8Weyl

instance Eq E8Root where
    (E8SpinRoot root1) == (E8SpinRoot root2) = root1 == root2
    (E8SRoot sign1) == (E8SRoot sign2) = (pad 8 sign1) == (pad 8 sign2)
    (E8SRoot _) == (E8SpinRoot _) = False
    (E8SpinRoot _) == (E8SRoot _) = False

negSign (Neg root) = negSign root
negSign root = False

instance Ord E8Root where
    (E8SpinRoot root1) `compare` (E8SpinRoot root2) = root1 `compare` root2
    (E8SpinRoot (SwapRoot i j)) `compare` (E8SRoot sign) | i==1 = GT
                                                         | at 1 sign == (-1) = GT
                                                         | otherwise = LT
    (E8SpinRoot (SignSwapRoot i j)) `compare` (E8SRoot sign) | i==1 = GT
                                                             | at 1 sign == (-1) = GT
                                                             | otherwise = LT
    (E8SpinRoot (Neg (SwapRoot i j))) `compare` (E8SRoot sign) | i==1 = LT
                                                               | at 1 sign == (-1) = GT
                                                               | otherwise = LT
    (E8SpinRoot (Neg (SignSwapRoot i j))) `compare` (E8SRoot sign) | i==1 = LT
                                                                   | at 1 sign == (-1) = GT
                                                                   | otherwise = LT
    (E8SRoot sign1) `compare` (E8SRoot sign2) = sign1 `compare` sign2
    s1 `compare` s2 = case s2 `compare` s1 of
                            EQ -> EQ
                            GT -> LT
                            LT -> GT

instance Root E8Root where
    coroot (E8SpinRoot root) = extendTo 0 1 8 $ coroot root
    coroot (E8SRoot signs) = scaleMatrix (1/2) $ toVector $ pad 8 signs

    positive (E8SpinRoot root) = positive root
    positive (E8SRoot signs) = (at 1 signs) == 1

    (E8SpinRoot root1) `reflect` (E8SpinRoot root2) = E8SpinRoot $ root1 `reflect` root2

    (E8SRoot sign) `reflect` (E8SpinRoot (SwapRoot i j)) = E8SRoot $ exchange i j $ sign
    (E8SRoot sign) `reflect` (E8SpinRoot (SignSwapRoot i j)) = E8SRoot $ dSwap i j $ exchange i j $ sign
    root1 `reflect` (E8SpinRoot (Neg root2)) = root1 `reflect` (E8SpinRoot root2)

    r@(E8SpinRoot (SwapRoot i j)) `reflect` (E8SRoot sign) | (at i sign) == (at j sign) = r
                                                           | (at i sign) == -1          = E8SRoot $ dSwap i j sign
                                                           | otherwise                  = E8SRoot $ dSwap i j $ neg $ pad 8 sign
    r@(E8SpinRoot (SignSwapRoot i j)) `reflect` (E8SRoot sign) | (at i sign) == -(at j sign) = r
                                                               | (at i sign) == -1           = E8SRoot $ dSwap i j sign
                                                               | otherwise                   = E8SRoot $ dSwap i j $ neg $ pad 8 $ sign
    (E8SpinRoot (Neg root)) `reflect` s@(E8SRoot sign) = RootSystem.negate $ (E8SpinRoot root) `reflect` s

    s@(E8SRoot sign1) `reflect` (E8SRoot sign2) | null disagree = E8SRoot $ neg $ pad 8 sign1
                                                | length disagree == 2 = E8SpinRoot $ root2 disagree
                                                | length disagree == 4 = s
                                                | length disagree == 6 = s `reflect` (E8SRoot (neg $ pad 8 sign2))
                                                | otherwise =  E8SRoot $ neg $ pad 8 sign1
        where disagree = disagreement sign1 sign2
              root2 disagree2 = if negative then (Neg root) else root
                      where [i,j] = disagree
                            sameSign = at i sign2 == at j sign2
                            negative = at i sign1 == -1
                            root = if sameSign then SignSwapRoot i j else SwapRoot i j

    negate (E8SpinRoot root) = E8SpinRoot $ RootSystem.negate root
    negate (E8SRoot sign) = E8SRoot $ neg $ pad 8 sign

    add root1 root2 | reflected == root1 = Nothing
                    | reflected == RootSystem.negate root1 = Nothing
                    | positive root2 && reflected < root1 = Nothing
                    | (not $ positive root2) && reflected > root1 = Nothing
                    | otherwise = Just reflected
        where reflected = root1 `reflect` root2

instance RootSystem E8System E8Root where
    rank _ = 8
    cartanAlgebra _ = fullSubAlgebra 8
    generators _ = E8SRoot (Signs.identity 8) : (map E8SpinRoot $ RootSystem.generators $ SpinSystem 7)

instance Arbitrary E8Root where
    arbitrary = do rootType <- arbitrary
                   spin <- arbitrary
                   sign <- arbitrary
                   return $ if rootType then E8SpinRoot spin else E8SRoot sign