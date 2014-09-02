module Test.TestSpin where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import RootSystem
import Spin

testSpin = testGroup "Spin algebras" [testSpinProp, testSpinUnit]

testSpinProp = testGroup "Properties" [testSpinReflectCoroot]
testSpinUnit = testGroup "Unit tests" []

bCoroot = BasicRoot . coroot

testSpinReflectCoroot = QC.testProperty "Coroot should push forward reflect to the reflect of basic roots" $ \(r1,r2) ->
    coroot ((r1::SpinRoot) `reflect` r2) == coroot ((bCoroot r1) `reflect` (bCoroot r2))