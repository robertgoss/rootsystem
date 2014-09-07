module Test.TestE7 where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix
import Data.Maybe


import Weyl
import RootSystem
import E7
import SemiSimple


testE7 = testGroup "E7 algebra" [testE7Prop, testE7Unit]

testE7Prop = testGroup "Properties" []
testE7Unit = testGroup "Unit tests" [testDetermineE7]


testDetermineE7 = testCase "Determine should give the correct type for e8 system" $ determine (E7System) @?= fromSimples 0 [E7]