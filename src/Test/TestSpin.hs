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
                                       testSpinMultiplyTorus]
testSpinUnit = testGroup "Unit tests" []

bCoroot = BasicRoot . coroot

padEqual v w = v' == w'
    where v' | ncols v >= ncols w = v
             | otherwise = v <|> zero 1 (ncols w - ncols v)
          w' | ncols w >= ncols v = w
             | otherwise = w <|> zero 1 (ncols v - ncols w)

testSpinReflectCoroot = QC.testProperty "Coroot should push forward reflect to the reflect of basic roots" $ \(r1,r2) ->
    coroot ((r1::SpinRoot) `reflect` r2) `padEqual` coroot ((bCoroot r1) `reflect` (bCoroot r2))

testSpinAddCoroot = QC.testProperty "Coroot should push forward add to the add of basic roots" $ \(r1,r2) ->
    isJust ((r1::SpinRoot) `add` r2) QC.==>
    (coroot . fromJust) (r1 `add` r2) `padEqual` coroot ((bCoroot r1) `reflect` (bCoroot r2))

testSpinEq =  QC.testProperty "Coroot should push forward equality to the reflect of basic roots" $ \(r1,r2) ->
    ((r1::SpinRoot) == r2) == ((bCoroot r1) == (bCoroot r2))

testSpinCmp =  QC.testProperty "Coroot should push forward ordering to the reflect of basic roots" $ \(r1,r2) ->
    ((r1::SpinRoot) `compare` r2) == ((bCoroot r1) `compare` (bCoroot r2))

testDetermineSpin = QC.testProperty "Determine should give the correct type for even spin system" $ \s ->
    determine (s::SpinSystem) `isomorphic` fromSimples 0 [D (RootSystem.rank s)]

testSpinMultiplyTorus = QC.testProperty "Torus representation should push forward  multilication to multiplication of basic elements" $ \(w1,w2) ->
    basicElement ((w1 :: SpinWeylElement) `multiply` w2) == (basicElement w1) `multiply` (basicElement w2)
