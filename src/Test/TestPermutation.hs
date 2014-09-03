module Test.TestPermutation where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix

import Permutation



testPerm = testGroup "Permutations" [testPermProp, testPermUnit]

testPermProp = testGroup "Properties" [testIdentityCombine, testCombineMatrix]
testPermUnit = testGroup "Unit tests" [testSwap, testAtOver]

testIdentityCombine = QC.testProperty "Identity should be unit of combine" $ \perm ->
    (Permutation.identity 1) `combine` perm == perm

fcombine = flip combine

testCombineMatrix = QC.testProperty "Flipped Combination should be push forward to matrix mulitplication" $ \(p,q) ->
    toMatrix (pad 10  (p `fcombine` q)) == (toMatrix (pad 10 p)) * (toMatrix (pad 10 q))

testSwap = testCase "test swap 1" $ at 1 perm @?= 2
    where perm = swap 1 2 $ Permutation.identity 2

testAtOver = testCase "test at a value larger than permutation" $ at 100 perm @?= 100
    where perm = swap 1 2 $ Permutation.identity 2