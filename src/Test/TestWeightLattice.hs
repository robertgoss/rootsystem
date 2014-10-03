module Test.TestWeightLattice where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import RootSystem
import SemiSimple
import WeightLattice

import E8

import Data.Maybe

testWeightLattice = testGroup "WeightLattice" [testWLProp, testWLUnit]

testWLProp = testGroup "Properties" [testWLAdjunctionId, testWLAdjunctionZero, testWLAssociateAddition]
testWLUnit = testGroup "Unit tests" []

rootSize ss = length . simpleRoots . rootSystem $ ss

testWLAdjunctionId = QC.testProperty "The inner product of the nth weight generator and the nth simple root should be 1" $ \ss ->
                    SemiSimple.rank ss < 9 QC.==>
                    all (==1) $ map (testWLAdjunction' ss) [(i,i) | i<-[1..(rootSize ss)]]

testWLAdjunctionZero = QC.testProperty "The inner product of the nth weight generator and the nth simple root should be 1" $ \ss ->
                    SemiSimple.rank ss < 9 QC.==>
                    all (==0) $ map (testWLAdjunction' ss) [(i,j) | i<-[1..(rootSize ss)], j<-[(i+1)..(rootSize ss)]]

testWLAdjunction' :: SemiSimple -> (Int, Int) -> Integer
testWLAdjunction' ss (i,j) = innerProduct wl (weights !! (i-1)) (sRoots !! (j-1)) 
    where sRoots = simpleRoots rs
          weights = WeightLattice.generators wl
          wl = basicLattice rs
          rs = rootSystem ss

e8Lattice = basicLattice E8System

testWLAssociateAddition = QC.testProperty "The weights associated to the sum of 2 roots should be the sum of the weights associated to each root" $ \(r1,r2) ->
                      isJust (r1 `RootSystem.add` r2) QC.==>
                      associatedWeight e8Lattice r1 `WeightLattice.add` associatedWeight e8Lattice r2 
                                           == associatedWeight e8Lattice (fromJust (r1 `RootSystem.add` r2))