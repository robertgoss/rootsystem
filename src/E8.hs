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
                     | E8Type3' Signs SpinWeylElement deriving (Eq,Show)

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
                          | sType == 2 = makeType2 sign (SpinElement (sign `combine` sign1) perm)
                          | sType == 4 = makeType3 False sign spin
                          | sType == 6 = makeType2 sign (SpinElement (neg $ pad 8 $ sign `combine` sign1) perm)
                          | sType == 8 = E8Type1 (SpinElement (neg $ pad 8 sign) perm)
    where sType = signType sign
sMult (E8Type3 sign spin) = makeType2 sign spin
sMult (E8Type3' sign spin) = sign2Mult tw $ makeType3 False (tw `combine` sign) spin
    where tw = twist sign

sign2Mult :: Signs2 -> E8WeylElement -> E8WeylElement
sign2Mult sign2 (E8Type1 (SpinElement sign perm)) = E8Type1 $ SpinElement (sign2 `combine` sign) perm
sign2Mult sign2 (E8Type2 sign spin) = makeType2 (sign2 `combine` sign) spin
sign2Mult sign2 (E8Type3 sign4 (SpinElement sign perm))
                | sType == 2 = makeType3 False sign4 $ SpinElement (sign2 `combine` sign4 `combine` sign) perm
                | sType == 6 = makeType3 False sign4 $ SpinElement (neg $ pad 8 $ sign2 `combine` sign4 `combine` sign) perm
                | sType == 4 && twType == 0 = makeType3 True sign4 $ SpinElement sign perm
                | sType == 4 && twType == 2 && combType == 2
                            = makeType3 True sign4 $ SpinElement (comb `combine` sign) perm
                | sType == 4 && twType == 2 && combType == 6
                            = makeType3 True sign4 $ SpinElement (neg $ pad 8 $ comb `combine` sign) perm
                | sType == 4 && twType == 4
                            = makeType3 True sign4 $ SpinElement (neg $ pad 8 $ comb `combine` sign) perm
    where sType = signType (sign2 `combine` sign4)
          tw = twist sign4
          twType = signType (sign2 `combine` tw)
          comb = sign2 `combine` tw `combine` sign4
          combType = signType comb
sign2Mult sign2 (E8Type3' sign4 spin) = sign2Mult tw $ sign2Mult sign2 $ E8Type3 sign4 spin
    where tw = twist sign4


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