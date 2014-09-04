module E8 where

import Data.Matrix

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
              | E8SRoot Signs deriving(Eq,Ord)


data E8System = E8System


data E8WeylElement = E8Type1 SpinWeylElement
                     | E8Type2 Signs SpinWeylElement
                     | E8Type3 Signs SpinWeylElement
                     | E8Type3' Signs SpinWeylElement

data E8Weyl = E8Weyl

instance Root E8Root where
    coroot (E8SpinRoot root) = extendTo 0 1 8 $ coroot root
    coroot (E8SRoot signs) = scaleMatrix (1/2) $ toVector $ pad 8 signs

    positive (E8SpinRoot root) = positive root
    positive (E8SRoot signs) = (at 1 signs) == 1

    (E8SpinRoot root1) `reflect` (E8SpinRoot root2) = E8SpinRoot $ root1 `reflect` root2

    (E8SpinRoot (SwapRoot i j)) `reflect` (E8SRoot sign) = E8SRoot $ exchange i j $ sign
    (E8SpinRoot (SignSwapRoot i j)) `reflect` (E8SRoot sign) = E8SRoot $ dSwap i j $ exchange i j $ sign
    (E8SpinRoot (Neg root1)) `reflect` root2 = (E8SpinRoot root1) `reflect` root2

    (E8SRoot sign) `reflect` r@(E8SpinRoot (SwapRoot i j)) | (at i sign) == (at j sign) = r
                                                           | otherwise                  = E8SRoot $ dSwap i j $ neg sign
    (E8SRoot sign) `reflect` r@(E8SpinRoot (SignSwapRoot i j)) | (at i sign) == -(at j sign) = r
                                                               | otherwise                   = E8SRoot $ dSwap i j $ neg sign
    s@(E8SRoot sign) `reflect` (E8SpinRoot (Neg root)) = RootSystem.negate $ s `reflect` (E8SpinRoot root)

    (E8SRoot sign1) `reflect` s@(E8SRoot sign2) | null disagree = E8SRoot $ neg sign1
                                                | length disagree == 2 = E8SpinRoot $ root2 disagree
                                                | length disagree == 4 = s
                                                | length disagree == 6 = (E8SRoot (neg sign1)) `reflect` s
                                                | otherwise =  E8SRoot $ neg sign1
        where disagree = disagreement sign1 sign2
              root2 disagree2 = if negative then (Neg root) else root
                      where [i,j] = disagree
                            sameSign = at i sign2 == at j sign2
                            negative = at i sign2 == -1
                            root = if sameSign then SignSwapRoot i j else SwapRoot i j

    negate (E8SpinRoot root) = E8SpinRoot $ RootSystem.negate root
    negate (E8SRoot sign) = E8SRoot $ neg sign

    add root1 root2 | reflected == root2 = Nothing
                    | reflected == RootSystem.negate root2 = Nothing
                    | reflected < root2 = Nothing
                    | otherwise = Just reflected
        where reflected = root1 `reflect` root2