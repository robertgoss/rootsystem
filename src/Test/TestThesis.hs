module Test.TestThesis where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Thesis
import Weyl


import Data.Set (fromList)

testThesis = testGroup "E8 algebra" [testThesisProp, testThesisUnit]

testThesisProp = testGroup "Properties" []
testThesisUnit = testGroup "Unit tests" [testSpin16QuotientElementNumber,
										testDoubleQuoE8Union,
										testDoubleQuoE8Distinct,
										testE8Bonds,
										testS3E7Gens,
										testS3E7Independence
										]

testSpin16QuotientElementNumber = testCase "The quotients of the weyl groups of E8 and Spin16 has order ?" $ 
                                    order spin16QuotientGroup @?= 135

testDoubleQuoE8Distinct = testCase "The S3E7 Spin16 double quotient at one and xs are distinct" $
                                    (one spin16QuotientGroup) `notElem` spin16X8SS3E7 @? "Should not contian one"

testDoubleQuoE8Union = testCase "The S3E7 Spin16 double quotient are generated by one and xs are distinct" $
                                    fromList (spin16OneS3E7 ++ spin16X8SS3E7) == (fromList $ Weyl.elements spin16QuotientGroup) @? "Should cover all"

e8bonds = [(0,1),(1,3),(2,3),(3,4),(4,5),(5,6),(6,7)]

bondCommute2 gens (i,j) = commute2 (gens!!i) (gens!!j)

testE8Bonds = testCase "The generators of the weyl group of E8 should commute according to bonds" $
								all (bondCommute2 (Weyl.generators e8Group)) e8bonds @? "Incorrect relations between generators"

testS3E7Gens = testCase "There should be 8 generators of S3E7" $ length (Weyl.generators s3e7Group) @?= 8

elementCommute gens i j = commute (gens!!i) (gens!!j)

testS3E7Independence = testCase "The S3 generator should commute with other generators" $ 
                                 all (elementCommute (Weyl.generators s3e7Group) 0) [1..7] @? "S3 generator should be independent from others"
