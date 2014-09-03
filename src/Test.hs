module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.TestRootSystem
import Test.TestCartanAlgebra
import Test.TestSemiSimple
import Test.TestSpin
import Test.TestPermutation

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testBasicRoot,
                           testOrthogonalBasis,
                           testRootSystem,
                           testWeylGroup,
                           testSpin,
                           testPerm]