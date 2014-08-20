module TestWeyl where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Weyl
import SemiSimple

testGenerate = testGroup "Basic Root" [testRootSystemProp, testRootSystemUnit]

testGenerate = testGroup "Properties" [testRootSystemRank,testRootSystemDim,testDetermineRootSystem]
testGenerate = testGroup "Unit tests" []