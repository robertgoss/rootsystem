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
                                    testSign2Mult, testSign4Mult, testSignMult,
                                    testPermMult, testSMult,
                                    testMultiply, testInverseL, testInverseR,
                                    testE8SimpleAction, testE8SimpleReflection]
testE8Unit = testGroup "Unit tests" [testDetermineE8]


testE8ReflectCoroot = QC.testProperty "Coroot should push forward reflect to the reflect of basic roots" $ \(r1,r2) ->
    coroot ((r1::E8Root) `reflect` r2) == coroot ((coroot r1) `reflect` (coroot r2))

testE8AddCoroot = QC.testProperty "Coroot should push forward add to the add of basic roots" $ \(r1,r2) ->
    isJust ((r1::E8Root) `add` r2) QC.==>
    coroot (fromJust (r1 `add` r2)) == fromJust (coroot r1 `add` coroot r2)

testE8Eq =  QC.testProperty "Coroot should push forward equality to the eq of basic roots" $ \(r1,r2) ->
    ((r1::E8Root) == r2) == (coroot r1 == coroot r2)

testE8Cmp =  QC.testProperty "Coroot should push forward ordering to the order of basic roots" $ \(r1,r2) ->
    ((r1::E8Root) `compare` r2) == (coroot r1 `compare` coroot r2)
 
torusMat = toMat . torusRepresentation
    where toMat (BasicElement m) = m

testPermMult = QC.testProperty "Should pushforward (permutation) multiplication to matrices" $ \(p,w) ->
    torusMat (permMult p w) == Permutation.toMatrix (Permutation.pad 8 p) * torusMat w

eType (E8Type1 _) = 1
eType (E8Type2 _ _) = 2
eType (E8Type3 _ _) = 3
eType (E8Type3' _ _) = 4

testSMult = QC.testProperty "Should pushforward (s-element) multiplication to matrices" $ \w ->
    eType w == 2 QC.==>
    torusMat (sMult w) == sMatrix * torusMat w

testSign2Mult = QC.testProperty "Should pushforward (sign 2) multiplication to matrices" $ \(s,w) ->
    signType s == 2 QC.==>
    torusMat (sign2Mult s w) == Signs.toMatrix (Signs.pad 8 s) * torusMat w

testSign4Mult = QC.testProperty "Should pushforward (sign 4) multiplication to matrices" $ \(s,w) ->
    signType s == 4 QC.==>
    torusMat (sign4Mult s w) == Signs.toMatrix (Signs.pad 8 s) * torusMat w

testSignMult = QC.testProperty "Should pushforward (sign) multiplication to matrices" $ \(s,w) ->
    torusMat (signMult s w) == Signs.toMatrix (Signs.pad 8 s) * torusMat w

testDetermineE8 = testCase "Determine should give the correct type for e8 system" $ determine (E8System) @?= fromSimples 0 [E8]

testMultiply = QC.testProperty "Should pushforward multiplication to matrices" $ \(g,w) ->
    torusRepresentation (g `multiply` (w::E8WeylElement)) == torusRepresentation g `multiply` torusRepresentation w

testInverseR = QC.testProperty "Should inverse should be right inverse to multiply" $ \(g) ->
    torusRepresentation g `multiply` torusRepresentation (Weyl.inverse g::E8WeylElement) == BasicElement (Data.Matrix.identity 8)

testInverseL = QC.testProperty "Should inverse should be right inverse to multiply" $ \(g) ->
    torusRepresentation (Weyl.inverse g::E8WeylElement) `multiply` torusRepresentation g == BasicElement (Data.Matrix.identity 8)

testE8SimpleAction = QC.testProperty "The weyl action of simple root is reflection" $ \(r1,r2) ->
    weylAction (simpleReflection r2 :: E8WeylElement) r1 == r1 `reflect` r2

testE8SimpleReflection = QC.testProperty "Simple reflection should push forward to basic simple reflection" $ \r ->
    torusRepresentation ((simpleReflection r) :: E8WeylElement) == simpleReflection (coroot r)
