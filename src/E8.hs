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
import qualified Permutation as Perm

type Signs0 = Signs
type Signs2 = Signs
type Signs4 = Signs
type Signs6 = Signs
type Signs8 = Signs



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

sMatrix = (Data.Matrix.identity 8) - fromList 8 8 (replicate 64 (1/4)) 

twist :: Signs4 -> Signs2
twist sign = dSwap firstPos firstNeg $ Signs.identity n
    where (Signs v) = sign
          n = nrows v
          firstPos = head $ filter (\i->at i sign==1) [1..]
          firstNeg = head $ filter (\i->at i sign==(-1)) [1..]

makeType2 sign (SpinElement spinSign perm) = E8Type2 plusSign $ SpinElement spinSign' perm
    where posSign = at 1 sign == 1
          plusSign = if posSign then sign else neg $ pad 8 sign
          spinSign' = if posSign then spinSign else neg $ pad 8 spinSign


makeType3 :: Bool -> Signs4 -> SpinWeylElement -> E8WeylElement
makeType3 tw sign4 (SpinElement spinSign perm) = if tw then E8Type3 plusSign $ SpinElement spinSign' perm
                                                       else E8Type3 plusSign $ SpinElement spinSign' perm
    where posSign = at 1 sign4 == 1
          plusSign = if posSign then sign4 else neg $ pad 8 sign4
          spinSign' = if posSign then spinSign else neg $ pad 8 spinSign



makeType3_s :: Signs -> Signs4 -> SpinWeylElement -> E8WeylElement
makeType3_s sign sign4 wspin@(SpinElement spinSign perm) | sType > 4 = makeType3_s (neg $ pad 8 sign) sign4 negWspin
                                                         | sType == 0 = makeType3 False sign4 wspin
                                                         | sType == 2 = makeType3_2 sign sign4 wspin
                                                         | sType == 4 = makeType3_4 sign sign4 wspin
    where sType = signType sign
          negWspin = SpinElement (neg $ pad 8 spinSign) perm

splitSign :: Signs4 -> (Signs2,Signs2)
splitSign sign4@(Signs v) = (sign2, sign4 `combine` sign2)
    where negpos = take 2 $ filter (\i->at i sign4==(-1)) [1..]
          sign2 = dSwap (negpos !! 1) (negpos !! 2) $ Signs.identity 8


makeType3_2 sign2 sign4 (SpinElement spinSign perm) | twisted = makeType3 True sign4 (SpinElement (spinSign `combine` shiftSign) perm)
                                                    | otherwise = makeType3 True sign4 (SpinElement (spinSign `combine` shiftSign) perm)
    where (twisted,shiftSign) = shiftType3 sign2 sign4


makeType3_4 sign4' sign4 (SpinElement spinSign perm) | twisted = makeType3 True sign4 (SpinElement (spinSign `combine` shiftSign) perm)
                                                     | otherwise = makeType3 True sign4 (SpinElement (spinSign `combine` shiftSign) perm)
    where (twisted1,shiftSign1) = shiftType3 sign2_1 sign4
          (twisted2,shiftSign2) = shiftType3 sign2_2 sign4
          twisted = twisted1 /= twisted2
          shiftSign = shiftSign1 `combine` shiftSign2
          (sign2_1,sign2_2) = splitSign sign4'


shiftType3 :: Signs2 -> Signs4 -> (Bool, Signs)
shiftType3 sign2 sign4 | contains = (False, sign2 `combine` sign4)
                       | excludes = (False, neg $ pad 8 $ sign2 `combine` sign4)
                       | isTwist = (True, Signs.identity 8)
                       | shiftedPlus = (True, sign2 `combine` tw `combine` sign4)
                       | shiftedNeg = (True, neg $ pad 8 $ sign2 `combine` tw `combine` sign4)
                       | otherwise = (True, neg $ pad 8 $ sign2 `combine` sign4)
    where contains = signType (sign2 `combine` sign4) == 2
          excludes = signType (sign2 `combine` sign4) == 6
          tw = twist sign4
          isTwist = signType (sign2 `combine` tw) == 0
          shifted = signType (sign2 `combine` tw) == 2
          shiftedPlus = shifted && signType (sign2 `combine` tw `combine` sign4) == 2
          shiftedNeg = shifted && signType (sign2 `combine` tw `combine` sign4) == 6


instance WeylGroupElement E8WeylElement E8Root where

    simpleReflection (E8SpinRoot root) = E8Type1 $ simpleReflection root
    simpleReflection (E8SRoot sign) = makeType2 sign $ SpinElement (Signs.identity 8) (Perm.identity 8)

    torusRepresentation (E8Type1 wspin) = torusRepresentation (spad 8 wspin)
    torusRepresentation (E8Type2 sign wspin) = signMatrix * sMatrix * spinMatrix
        where spinMatrix = torusRepresentation (spad 8 wspin)
              signMatrix = toMatrix (pad 8 sign)
    torusRepresentation (E8Type3 sign wspin) = sMatrix * signMatrix * sMatrix * spinMatrix
        where spinMatrix = torusRepresentation (spad 8 wspin)
              signMatrix = toMatrix (pad 8 sign)
    torusRepresentation (E8Type3' sign wspin) = twSignMatrix * sMatrix * signMatrix * sMatrix * spinMatrix
        where spinMatrix = torusRepresentation (spad 8 wspin)
              signMatrix = toMatrix (pad 8 sign)
              twSignMatrix = toMatrix (twist $ pad 8 sign)

    inverse (E8Type1 wspin) = E8Type1 $ inverse wspin
    inverse (E8Type2 sign wspin) = makeType2 ispinSign $ SpinElement (permute iperm sign) iperm
        where (SpinElement ispinSign iperm) = inverse wspin
    inverse (E8Type3 sign4 wspin) = makeType3_s ispinSign (permute iperm sign4) $ SpinElement (Signs.identity 8) iperm
        where (SpinElement ispinSign iperm) = inverse wspin
    inverse (E8Type3' sign4 wspin) = makeType3_s ispinSign (permute iperm sign4) $ SpinElement (permute iperm tw) iperm
        where (SpinElement ispinSign iperm) = inverse wspin
              tw = twist sign4


instance Arbitrary E8Root where
    arbitrary = do rootType <- arbitrary
                   spin <- arbitrary
                   sign <- arbitrary
                   return $ if rootType then E8SpinRoot spin else E8SRoot sign