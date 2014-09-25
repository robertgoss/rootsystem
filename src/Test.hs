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
import Test.TestSigns
import Test.TestE8
import Test.TestE7
import Test.TestWeyl

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testBasicRoot,
                           testOrthogonalBasis,
                           testRootSystem,
                           testWeylGroup,
                           testWeyl,
                           testSpin,
                           testPerm,
                           testSigns,
                           testE8,
                           testE7]