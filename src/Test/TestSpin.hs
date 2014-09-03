module Test.TestSpin where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix
import Data.Maybe

import RootSystem
import Spin
import SemiSimple

testSpin = testGroup "Spin algebras" [testSpinProp, testSpinUnit]

testSpinProp = testGroup "Properties" [testSpinReflectCoroot, testSpinAddCoroot,testDetermineSpin]
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

testDetermineSpin = QC.testProperty "Determine should give the correct type for even spin system" $ \s ->
    determine (s::SpinSystem) == fromSimples 0 [D (RootSystem.rank s)]