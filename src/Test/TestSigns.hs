module Test.TestSigns where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Signs

testSigns = testGroup "Spin algebras" [testSignsProp, testSignsUnit]

testSignsProp = testGroup "Properties" [testCombineToMatrix, testIdentityUnit]
testSignsUnit = testGroup "Unit tests" [testSwap1, testSwap2, testSwap3, testAtOver]


testCombineToMatrix = QC.testProperty "Combinations should push forward to matrix multiplication" $ \(s,t) ->
        toMatrix (pad 10 $ (s :: Signs) `combine` t) == (toMatrix (pad 10 s)) * (toMatrix (pad 10 t))

testIdentityUnit = QC.testProperty "Identity should be unit of combination" $ \s ->
        s `combine` (identity 1) == s


testSwap1 = testCase "Test Swap 1" $ at 1 s @?= 1
    where s = dSwap 2 3 $ identity 1

testSwap2 = testCase "Test Swap 2" $ at 2 s @?= -1
    where s = dSwap 2 3 $ identity 1

testSwap3 = testCase "Test Swap 3" $ at 3 s @?= -1
    where s = dSwap 2 3 $ identity 1

testAtOver = testCase "Test at when over sign size" $ at 100 s @?= 1
    where s = dSwap 2 3 $ identity 1
