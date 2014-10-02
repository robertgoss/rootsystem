module Test.TestSpin where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix
import Data.Maybe

import Weyl
import RootSystem
import Spin
import SemiSimple

testSpin = testGroup "Spin algebras" [testSpinProp, testSpinUnit]

testSpinProp = testGroup "Properties" [testSpinReflectCoroot, testSpinAddCoroot, testSpinEq, testSpinCmp, testDetermineSpin,
                                       testSpinMultiplyTorus, testSpinInverse, testSpinOrder, testSpinSimpleAction,
                                       testSpinSimpleReflection]
testSpinUnit = testGroup "Unit tests" []



testSpinReflectCoroot = QC.testProperty "Coroot should push forward reflect to the reflect of basic roots" $ \(r1,r2) ->
    coroot ((r1::SpinRoot) `reflect` r2) == coroot r1 `reflect` coroot r2

testSpinAddCoroot = QC.testProperty "Coroot should push forward add to the add of basic roots" $ \(r1,r2) ->
    isJust ((r1::SpinRoot) `add` r2) QC.==>
    (coroot . fromJust) (r1 `add` r2) == fromJust (coroot r1 `add` coroot r2)

testSpinEq =  QC.testProperty "Coroot should push forward equality to the eq of basic roots" $ \(r1,r2) ->
    ((r1::SpinRoot) == r2) == (coroot r1 == coroot r2)

testSpinCmp =  QC.testProperty "Coroot should push forward ordering to the order of basic roots" $ \(r1,r2) ->
    ((r1::SpinRoot) `compare` r2) == (coroot r1 `compare` coroot r2)

testDetermineSpin = QC.testProperty "Determine should give the correct type for even spin system" $ \s ->
    determine (s::SpinSystem) `isomorphic` fromSimples 0 [D (RootSystem.rank s)]

testSpinMultiplyTorus = QC.testProperty "Torus representation should push forward  multilication to multiplication of basic elements" $ \(w1,w2) ->
    torusRepresentation ((w1 :: SpinWeylElement) `multiply` w2) == torusRepresentation w1 `multiply` torusRepresentation w2

testSpinInverse = QC.testProperty "Inverse pushes forward to basic root inverse" $ \w ->
    torusRepresentation (inverse (w::SpinWeylElement)) == inverse (torusRepresentation w) 

testSpinOrder = QC.testProperty "Test the order of spin weyl group" $ \n ->
    (order . SpinWeyl . small) n == weylOrder (fromSimples 0 [D (small n)])
    where small m = abs $ m `mod` 6

testSpinSimpleAction = QC.testProperty "The weyl action of simple root is reflection" $ \(r1,r2) ->
    weylAction (simpleReflection r2 :: SpinWeylElement) r1 == r1 `reflect` r2

testSpinSimpleReflection = QC.testProperty "Simple reflection should push forward to basic simple reflection" $ \r ->
    torusRepresentation ((simpleReflection r) :: SpinWeylElement) == simpleReflection (coroot r)