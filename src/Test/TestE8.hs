module Test.TestE8 where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix
import Data.Maybe

import Signs
import Weyl
import RootSystem
import E8
import SemiSimple
import Permutation

testE8 = testGroup "E8 algebra" [testE8Prop, testE8Unit]

testE8Prop = testGroup "Properties" [testE8ReflectCoroot, testE8AddCoroot, testE8Eq, testE8Cmp,
                                    testSign2Mult, testSign4Mult, testSignMult]
testE8Unit = testGroup "Unit tests" [testDetermineE8]

bCoroot = BasicRoot . coroot

padEqual v w = v' == w'
    where v' | ncols v >= ncols w = v
             | otherwise = v <|> zero 1 (ncols w - ncols v)
          w' | ncols w >= ncols v = w
             | otherwise = w <|> zero 1 (ncols v - ncols w)

testE8ReflectCoroot = QC.testProperty "Coroot should push forward reflect to the reflect of basic roots" $ \(r1,r2) ->
    coroot ((r1::E8Root) `reflect` r2) `padEqual` coroot ((bCoroot r1) `reflect` (bCoroot r2))

testE8AddCoroot = QC.testProperty "Coroot should push forward add to the add of basic roots" $ \(r1,r2) ->
    isJust ((r1::E8Root) `add` r2) QC.==>
    (coroot . fromJust) (r1 `add` r2) `padEqual` (coroot .fromJust) ((bCoroot r1) `add` (bCoroot r2))

testE8Eq =  QC.testProperty "Coroot should push forward equality to the eq of basic roots" $ \(r1,r2) ->
    ((r1::E8Root) == r2) == ((bCoroot r1) == (bCoroot r2))

testE8Cmp =  QC.testProperty "Coroot should push forward ordering to the order of basic roots" $ \(r1,r2) ->
    ((r1::E8Root) `compare` r2) == ((bCoroot r1) `compare` (bCoroot r2))
 
testPermMult = QC.testProperty "Should pushforward (permutation) multiplication to matrices" $ \(p,w) ->
    torusRepresentation (permMult p w) == Permutation.toMatrix (Permutation.pad 8 p) * torusRepresentation w

testSMult = QC.testProperty "Should pushforward (s-element) multiplication to matrices" $ \w ->
    torusRepresentation (sMult w) == sMatrix * torusRepresentation w

eType (E8Type1 _) = 1
eType (E8Type2 _ _ ) = 2
eType (E8Type3 _ _ ) = 3
eType (E8Type3' _ _) = 4

testSign2Mult = QC.testProperty "Should pushforward (sign 2) multiplication to matrices" $ \(s,w) ->
    signType s == 2 QC.==>
    torusRepresentation (sign2Mult s w) == Signs.toMatrix (Signs.pad 8 s) * torusRepresentation w

testSign4Mult = QC.testProperty "Should pushforward (sign 4) multiplication to matrices" $ \(s,w) ->
    signType s == 4 QC.==>
    torusRepresentation (sign2Mult s w) == Signs.toMatrix (Signs.pad 8 s) * torusRepresentation w

testSignMult = QC.testProperty "Should pushforward (sign) multiplication to matrices" $ \(s,w) ->
    torusRepresentation (sign2Mult s w) == Signs.toMatrix (Signs.pad 8 s) * torusRepresentation w

testDetermineE8 = testCase "Determine should give the correct type for e8 system" $ determine (E8System) @?= fromSimples 0 [E8]