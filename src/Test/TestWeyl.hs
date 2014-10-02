module Test.TestWeyl where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Weyl
import RootSystem
import SemiSimple

testWeyl = testGroup "Basic Weyl" [testWeylProp, testWeylUnit]

testWeylProp = testGroup "Properties" [testWeylSimpleAction]
testWeylUnit = testGroup "Unit tests" []

testWeylSimpleAction = QC.testProperty "The weyl action of simple root is reflection" $ \(r1,r2) ->
    weylAction (simpleReflection r2 :: BasicWeylGroupElement) r1 == r1 `reflect` r2
