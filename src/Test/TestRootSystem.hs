module Test.TestRootSystem(testBasicRoot) where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix
import Data.Ratio

import RootSystem

testBasicRoot = testGroup "Basic Root" [testBasicRootProp, testBasicRootUnit]

testBasicRootProp = testGroup "Properties" [testReflectIdo]
testBasicRootUnit = testGroup "Unit tests" [testReflect1]

root3 a b c = BasicRoot (fromList 1 3 [a,b,c])

testReflectIdo = QC.testProperty "(x `reflect` y) `reflect` y == x" $ \r1 r2->
    isNonZero r1 && isNonZero r2 QC.==> 
    (r1 `reflect` r2) `reflect` r2 == (r1 :: BasicRoot)

testReflect1 = testCase "Test reflection 1" $ r1 `reflect` r2  @?= r3
    where r1 = (BasicRoot (fromList 1 4 [1,2,3,1%2]))
          r2 = (BasicRoot (fromList 1 4 [1,0,1,0]))
          r3 = (BasicRoot (fromList 1 4 [-3,2,-1,1%2]))
