module Test.TestSemiSimple where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix

import Weyl as W
import RootSystem as RS
import SemiSimple as SS



testRootSystem = testGroup "Root System" [testRootSystemProp, testRootSystemUnit]

testRootSystemProp = testGroup "Properties" [testRootSystemRank,testRootSystemDim,testDetermineRootSystem]
testRootSystemUnit = testGroup "Unit tests" []


testRootSystemRank = QC.testProperty "The rank of the root system should be the same as the rank of the algebra" $ \semi ->
    RS.rank (rootSystem semi) == SS.rank semi


testRootSystemDim = QC.testProperty "The dimension of the root system should be the same as the dimension of the algebra" $ \semi ->
   RS.dim (rootSystem semi) == SS.dim semi


testDetermineRootSystem = QC.testProperty "Determine should be partial inverse to rootSystem" $ \semi ->
   determine (rootSystem semi) == semi



testWeylGroup = testGroup "Weyl Group" [testWeylGroupProp, testWeylGroupUnit]

testWeylGroupProp = testGroup "Properties" [testWeylGroupOrder]
testWeylGroupUnit = testGroup "Unit  tests" []

testWeylGroupOrder = QC.testProperty "The order of the weyl group should be the weyl order" $ \semi ->
    SS.rank semi < 7 QC.==>
    W.order (SS.weylGroup semi) == SS.weylOrder semi