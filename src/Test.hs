module Main where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Test.TestRootSystem

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [testBasicRoot]