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
import SubGroup
import Quotient
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
                     | E8Type3' Signs SpinWeylElement deriving (Eq,Show)

data E8Weyl = E8Weyl

data E8SubE7 = E8SubE7
data E8SubS3E7 = E8SubS3E7
data E8SubS3S1E6 = E8SubS3S1E6
data E8Spin16Quotient = E8Spin16Quotient

data E8SubS3E7System = E8SubS3E7System
data E8SubSpin16System = E8SubSpin16System

instance Eq E8Root where
    (E8SpinRoot root1) == (E8SpinRoot root2) = root1 == root2
    (E8SRoot sign1) == (E8SRoot sign2) = pad 8 sign1 == pad 8 sign2
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
    coroot (E8SpinRoot root) = coroot root
    coroot (E8SRoot signs) = BasicRoot $ scaleMatrix (1/2) $ toVector $ pad 8 signs

    positive (E8SpinRoot root) = positive root
    positive (E8SRoot signs) = at 1 signs == 1

    (E8SpinRoot root1) `reflect` (E8SpinRoot root2) = E8SpinRoot $ root1 `reflect` root2

    (E8SRoot sign) `reflect` (E8SpinRoot (SwapRoot i j)) = E8SRoot $ exchange i j sign
    (E8SRoot sign) `reflect` (E8SpinRoot (SignSwapRoot i j)) = E8SRoot $ dSwap i j $ exchange i j sign
    root1 `reflect` (E8SpinRoot (Neg root2)) = root1 `reflect` E8SpinRoot root2

    r@(E8SpinRoot (SwapRoot i j)) `reflect` (E8SRoot sign) | at i sign == at j sign = r
                                                           | at i sign == -1          = E8SRoot $ dSwap i j sign
                                                           | otherwise                = E8SRoot $ dSwap i j $ neg $ pad 8 sign
    r@(E8SpinRoot (SignSwapRoot i j)) `reflect` (E8SRoot sign) | at i sign == -(at j sign) = r
                                                               | at i sign == -1           = E8SRoot $ dSwap i j sign
                                                               | otherwise                 = E8SRoot $ dSwap i j $ neg $ pad 8 sign
    (E8SpinRoot (Neg root)) `reflect` s@(E8SRoot sign) = RootSystem.negate $ E8SpinRoot root `reflect` s

    s@(E8SRoot sign1) `reflect` (E8SRoot sign2) | null disagree = E8SRoot $ neg $ pad 8 sign1
                                                | length disagree == 2 = E8SpinRoot $ root2 disagree
                                                | length disagree == 4 = s
                                                | length disagree == 6 = s `reflect` E8SRoot (neg $ pad 8 sign2)
                                                | otherwise =  E8SRoot $ neg $ pad 8 sign1
        where disagree = disagreement sign1 sign2
              root2 disagree2 = if negative then Neg root else root
                      where [i,j] = disagree
                            sameSign = at i sign2 == at j sign2
                            negative = at i sign1 == -1
                            root = if sameSign then SignSwapRoot i j else SwapRoot i j

    negate (E8SpinRoot root) = E8SpinRoot $ RootSystem.negate root
    negate (E8SRoot sign) = E8SRoot $ neg $ pad 8 sign

    add root1 root2 | reflected == root1 = Nothing
                    | reflected == RootSystem.negate root1 = Nothing
                    | positive root2 && reflected < root1 = Nothing
                    | not (positive root2) && reflected > root1 = Nothing
                    | otherwise = Just reflected
        where reflected = root1 `reflect` root2

sAct :: E8Root -> E8Root
sAct r = r `reflect` (E8SRoot $ Signs.identity 8)

signAct :: Signs -> E8Root -> E8Root
signAct sign (E8SpinRoot root) = E8SpinRoot $ weylAction (SpinElement sign (Perm.identity 8)) root
signAct sign (E8SRoot ssign) = E8SRoot $ sign `combine` ssign

spinAct :: SpinWeylElement -> E8Root -> E8Root
spinAct spin (E8SpinRoot root) = E8SpinRoot $ weylAction spin root
spinAct (SpinElement sign perm) (E8SRoot ssign) = E8SRoot $ sign `combine` permute perm ssign 

instance RootSystem E8System E8Root where
    rank _ = 8
    cartanAlgebra _ = fullSubAlgebra 8
    generators _ = E8SRoot (Signs.identity 8) : map E8SpinRoot (RootSystem.generators $ SpinSystem 7)

instance SubRootSystem E8SubSpin16System E8System E8Root where
    ambientSystem _ = E8System
    subGenerators = E8SpinRoot (SwapRoot 7 8) : tail (RootSystem.generators E8System)

instance SubRootSystem E8SubS3E7System E8System E8Root where
    ambientSystem _ = E8System
    subGenerators = E8SpinRoot (SignSwapRoot 7 8) : init (RootSystem.generators E8System)

sMatrix = Data.Matrix.identity 8 - fromList 8 8 (replicate 64 (1/4)) 

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
makeType3 tw sign4 (SpinElement spinSign perm) = if tw then E8Type3' plusSign $ SpinElement spinSign' perm
                                                       else E8Type3 plusSign $ SpinElement spinSign' perm
    where posSign = at 1 sign4 == 1
          plusSign = if posSign then sign4 else neg $ pad 8 sign4
          spinSign' = if posSign then spinSign else neg $ pad 8 spinSign

permSpin perm1 (SpinElement sign perm2) = SpinElement (permute perm1 sign) (perm2 `Perm.combine` perm1)

permMult :: Perm.Permutation -> E8WeylElement -> E8WeylElement
permMult perm (E8Type1 spin) = E8Type1 $ permSpin perm spin
permMult perm (E8Type2 sign spin) = makeType2 (permute perm sign) (permSpin perm spin)
permMult perm (E8Type3 sign4 spin) = makeType3 False (permute perm sign4) (permSpin perm spin)
permMult perm (E8Type3' sign4 spin) = sign2Mult (permute perm tw) $ makeType3 False (permute perm sign4) (permSpin perm spin)
    where tw = twist sign4




sMult :: E8WeylElement -> E8WeylElement
sMult (E8Type1 spin) = makeType2 (Signs.identity 8) spin
sMult (E8Type2 sign spin@(SpinElement sign1 perm))
                          | sType == 0 = E8Type1 spin
                          | sType == 2 = makeType2 sign (SpinElement (sign `combine` signSwap1) signSwapP)
                          | sType == 4 = makeType3 False sign spin
                          | sType == 6 = sign8Mult $ makeType2 sign' (SpinElement (sign' `combine` signSwap1') signSwapP')
                          | sType == 8 = E8Type1 (SpinElement (neg $ pad 8 sign) perm)
    where sType = signType sign
          signSwap1 = permute (sign2perm sign) sign1
          signSwapP = perm `Perm.combine` sign2perm sign
          sign' = neg $ pad 8 sign
          signSwap1' = permute (sign2perm sign') sign1
          signSwapP' = perm `Perm.combine` sign2perm sign'
sMult (E8Type3 sign spin) = makeType2 sign spin
sMult (E8Type3' sign spin) = signMult tw $ sMult $ signMult tw $ permMult spinSwap $ makeType2 sign spin
    where tw = twist sign
          spinSwap = sign2perm tw

sign2perm :: Signs2 -> Perm.Permutation
sign2perm (Signs v) = Perm.swap i j $ Perm.identity 8
    where [i,j] = take 2 $ map snd $ filter (\(a,b)->a==(-1)) $ zip (toList v) [1..]

sign2Mult :: Signs2 -> E8WeylElement -> E8WeylElement
sign2Mult sign2 (E8Type1 (SpinElement sign perm)) = E8Type1 $ SpinElement (sign2 `combine` sign) perm
sign2Mult sign2 (E8Type2 sign spin) = makeType2 (sign2 `combine` sign) spin
sign2Mult sign2 (E8Type3 sign4 (SpinElement sign perm))
                | signType mid == 2 = sign2Mult_2 sign2 sign4 sign perm
                | signType mid == 6 = sign2Mult_2 sign2 (neg $ pad 8 sign4) (neg $ pad 8 sign) perm
                | signType mid == 4 = sign2Mult_4 sign2 sign4 sign perm
    where mid = sign2 `combine` permute (sign2perm sign2) sign4
sign2Mult sign2 (E8Type3' sign4 spin)
                | signType twSign == 0 = makeType3 False sign4 spin
                | signType twSign == 2 = sign2Mult twSign (E8Type3 sign4 spin)
                | signType twSign == 4 = sign2Mult sign2pos $ sign2Mult sign2neg $ E8Type3 sign4 spin
    where twSign = sign2 `combine` twist sign4
          (sign2pos, sign2neg) = splitSign4 twSign

sign2Mult_2 sign2 sign4 sign perm = makeType3 False sign4' $ SpinElement (mid `combine` sign') perm'
    where mid = sign2 `combine` sign4'
          sign4' = permute sign2swap sign4
          sign2swap =  sign2perm sign2
          midSwap = sign2perm mid
          sign' = permute midSwap $ permute sign2swap sign
          perm' = (perm `Perm.combine` sign2swap) `Perm.combine` midSwap

sign2Mult_4 sign2 sign4 sign perm | signType twsign2 == 0 = makeType3 True sign4 $ SpinElement sign perm
                                  | signType twsign2 == 2 = sign2Mult tw $ sign2Mult twsign2 $ E8Type3 sign4 $ SpinElement sign perm
                                  | signType twsign2 == 4 = sign2Mult sign2pos $ sign2Mult sign2neg $ E8Type3 sign4 $ SpinElement sign perm
    where tw = twist sign4
          twsign2 = tw `combine` sign2
          (Signs v2) = sign2
          [i,j] = take 2 $ map snd $ filter (\(a,b)-> a==(-1) ) $ zip (toList v2) [1..]
          sign2pos = dSwap 1 i $ Signs.identity 8
          sign2neg = dSwap 1 j $ Signs.identity 8


splitSign4 sign4@(Signs v4) = (sign2, sign4 `combine` sign2)
    where [i,j] = take 2 $ map snd $ filter (\(a,b)-> a==(-1) ) $ zip (toList v4) [1..]
          sign2 = dSwap i j $ Signs.identity 8

sign4Mult :: Signs4 -> E8WeylElement -> E8WeylElement
sign4Mult sign4 w = sign2Mult sign2p $ sign2Mult sign2m w
    where (sign2p,sign2m) = splitSign4 sign4

sign6Mult :: Signs6 -> E8WeylElement -> E8WeylElement
sign6Mult sign6 w = sign8Mult $ sign2Mult (neg $ pad 8 sign6) w

sign8Mult :: E8WeylElement -> E8WeylElement
sign8Mult (E8Type1 (SpinElement sign perm)) = E8Type1$ SpinElement (neg $ pad 8 sign) perm
sign8Mult (E8Type2 sign spin) = makeType2 (neg $ pad 8 sign) spin
sign8Mult (E8Type3 sign4 spin) = makeType3 False (neg $ pad 8 sign4) spin
sign8Mult (E8Type3' sign4 spin) = makeType3 True (neg $ pad 8 sign4) spin

signMult :: Signs -> E8WeylElement -> E8WeylElement
signMult sign w | signType sign == 0 = w
                | signType sign == 2 = sign2Mult sign w
                | signType sign == 4 = sign4Mult sign w
                | signType sign == 6 = sign6Mult sign w
                | signType sign == 8 = sign8Mult w

e8Unit :: E8WeylElement
e8Unit = E8Type1 $ SpinElement (Signs.identity 8) (Perm.identity 8)



instance WeylGroupElement E8WeylElement E8Root where

    simpleReflection (E8SpinRoot root) = E8Type1 $ simpleReflection root
    simpleReflection (E8SRoot sign) = makeType2 sign $ SpinElement sign (Perm.identity 8)

    torusRepresentation (E8Type1 wspin) = torusRepresentation (spad 8 wspin)
    torusRepresentation (E8Type2 sign wspin) = BasicElement $ signMatrix * sMatrix * spinMatrix
        where spinMatrix = toMat $ torusRepresentation (spad 8 wspin)
              signMatrix = toMatrix (pad 8 sign)
              toMat (BasicElement m) = m
    torusRepresentation (E8Type3 sign wspin) = BasicElement $ sMatrix * signMatrix * sMatrix * spinMatrix
        where spinMatrix = toMat $ torusRepresentation (spad 8 wspin)
              signMatrix = toMatrix (pad 8 sign)
              toMat (BasicElement m) = m
    torusRepresentation (E8Type3' sign wspin) = BasicElement $ twSignMatrix * sMatrix * signMatrix * sMatrix * spinMatrix
        where spinMatrix = toMat $ torusRepresentation (spad 8 wspin)
              signMatrix = toMatrix (pad 8 sign)
              twSignMatrix = toMatrix (twist $ pad 8 sign)
              toMat (BasicElement m) = m

    inverse (E8Type1 wspin) = E8Type1 $ inverse wspin
    inverse (E8Type2 sign (SpinElement sSign perm)) = permMult iperm $ signMult sSign $ sMult $ signMult sign e8Unit
        where iperm = Perm.inverse perm
    inverse (E8Type3 sign (SpinElement sSign perm)) = permMult iperm $ signMult sSign $ sMult $ signMult sign $ sMult e8Unit
        where iperm = Perm.inverse perm
    inverse (E8Type3' sign (SpinElement sSign perm)) = permMult iperm $ signMult sSign $ sMult $ signMult sign $ sMult $ signMult tw e8Unit
        where iperm = Perm.inverse perm
              tw = twist sign

    multiply (E8Type1 (SpinElement sSign perm)) g = signMult sSign $ permMult perm g
    multiply (E8Type2 sign (SpinElement sSign perm)) g = signMult sign $ sMult $ signMult sSign $ permMult perm g
    multiply (E8Type3 sign (SpinElement sSign perm)) g = sMult $ signMult sign $ sMult $ signMult sSign $ permMult perm g
    multiply (E8Type3' sign (SpinElement sSign perm)) g = signMult tw $ sMult $ signMult sign $ sMult $ signMult sSign $ permMult perm g
        where tw = twist sign

    weylAction (E8Type1 spin) root = spinAct spin root
    weylAction (E8Type2 sign spin) root = signAct sign $ sAct $ spinAct spin root
    weylAction (E8Type3 sign spin) root = sAct $ signAct sign $ sAct $ spinAct spin root
    weylAction (E8Type3' sign spin) root = signAct tw $ sAct $ signAct sign $ sAct $ spinAct spin root
        where tw = twist sign
 
leftDominate EQ a = a
leftDominate b  _ = b

instance Ord E8WeylElement where
    (E8Type1 spin1) `compare` (E8Type1 spin2) = spin1 `compare` spin2
    (E8Type1 _) `compare` _ = LT
    (E8Type2 sign1 spin1) `compare` (E8Type2 sign2 spin2) = leftDominate (sign1 `compare` sign2 ) (spin1 `compare` spin2)
    (E8Type2 _ _) `compare` (E8Type1 _) = GT
    (E8Type2 _ _) `compare` _ = LT
    (E8Type3 sign1 spin1) `compare` (E8Type3 sign2 spin2) = leftDominate (sign1 `compare` sign2 ) (spin1 `compare` spin2)
    (E8Type3 _ _) `compare` (E8Type3' _ _) = LT
    (E8Type3 _ _) `compare` _ = GT
    (E8Type3' sign1 spin1) `compare` (E8Type3' sign2 spin2) = leftDominate (sign1 `compare` sign2 ) (spin1 `compare` spin2)
    (E8Type3' _ _) `compare` _ = GT

instance WeylGroup E8Weyl E8WeylElement E8System E8Root where
    one _ = e8Unit
    generators _ = map simpleReflection $ RootSystem.generators E8System
    weylGroup _ = E8Weyl

instance QuotientWeylGroup E8Spin16Quotient E8Weyl E8WeylElement E8System E8Root where
    superGroup _ = E8Weyl
    quoWeylEq _ (E8Type1 _) (E8Type1 _) = True
    quoWeylEq _ (E8Type1 _) _ = False
    quoWeylEq _ (E8Type2 sign1 _) (E8Type2 sign2 _) = sign1 == sign2
    quoWeylEq _ (E8Type2 _ _) _ = False
    quoWeylEq _ (E8Type3 sign1 _) (E8Type3 sign2 _) = sign1 == sign2
    quoWeylEq _ (E8Type3 _ _) _ = False
    quoWeylEq _ (E8Type3' sign1 _) (E8Type3' sign2 _) = sign1 == sign2
    quoWeylEq _ (E8Type3' _ _) _ = False

instance QuotientWeylGroupCmp E8Spin16Quotient E8Weyl E8WeylElement E8System E8Root where
    quoWeylCmp q a b = if quoWeylEq q a b then EQ else a `compare` b

instance SubWeylGroup E8SubE7 E8Weyl E8WeylElement E8System E8Root where
    ambientGroup _ = E8Weyl
    subGenerators _ = init $ Weyl.generators E8Weyl

instance SubWeylGroup E8SubS3E7 E8Weyl E8WeylElement E8System E8Root where
    ambientGroup _ = E8Weyl
    subGenerators _ = s3gen : init (Weyl.generators E8Weyl)
      where s3gen = E8Type1 $ SpinElement (Signs.identity 8) (Perm.swap 7 8 $ Perm.identity 8)

instance SubWeylGroup E8SubS3S1E6 E8Weyl E8WeylElement E8System E8Root where
    ambientGroup _ = E8Weyl
    subGenerators _ = s3gen : take 6 (Weyl.generators E8Weyl)
      where s3gen = E8Type1 $ SpinElement (Signs.identity 8) (Perm.swap 7 8 $ Perm.identity 8)

instance Arbitrary E8Root where
    arbitrary = do rootType <- arbitrary
                   spin <- arbitrary
                   sign <- arbitrary
                   return $ if rootType then E8SpinRoot spin else E8SRoot sign

instance Arbitrary E8WeylElement where
    arbitrary = do spin <- arbitrary
                   perm <- arbitrary
                   i <- arbitrary
                   let sign2 = dSwap 1 2 $ Signs.identity 8
                       sign4 = dSwap 1 2 $ dSwap 3 4 $ Signs.identity 8
                       types = [E8Type1 spin,
                                makeType2 (permute perm sign2)  spin,
                                makeType3 False (permute perm sign4) spin,
                                makeType3 True (permute perm sign4) spin]
                   return $ types !! (i `mod` 4)