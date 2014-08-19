module Test.TestCartanAlgebra where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix

import Rational
import RootSystem
import CartanAlgebra

testOrthogonalBasis = testGroup "Orthogonal Basis" [testOrthogonalBasisProp, testOrthogonalBasisUnit]

testOrthogonalBasisProp = testGroup "Properties" [testOrthogonalality, testOrthIdopotent, testOrthRepeat, testOrthNonZero]
testOrthogonalBasisUnit = testGroup "Unit tests" [testOrth1]

makeBasis :: [Vector QQ] -> [Vector QQ]
makeBasis vecs = orthogonalBasis . CartanAlgebra.span $ vecs

testOrthogonalality = QC.testProperty "Orthogonal Basis should be orthogonal" $ \roots ->
        testOrthogonal . makeBasis . (map coroot) $ (roots::[BasicRoot])

testOrthIdopotent = QC.testProperty "Orthogonal basis should be ideopotent" $ \roots ->
        (makeBasis . makeBasis) (map coroot (roots::[BasicRoot])) == makeBasis (map coroot roots)

testOrthRepeat = QC.testProperty "Orthogonal basis should be reduce a repeated root" $ \root ->
        isNonZero root QC.==>
        makeBasis (map coroot [root,root,root]) == [coroot (root::BasicRoot)]

testOrthNonZero = QC.testProperty "Orthogonal basis should be non zero" $ \roots -> 
        all (isNonZero . BasicRoot) $ makeBasis (map coroot (roots::[BasicRoot]))

testOrth1 = testCase "Orthogonal basis 1" $ makeBasis vs @?= ws
        where vs = map (fromList 1 4) [[1,0,0,1],[1,2,3,4],[2,2,3,5]]
              ws = map (fromList 1 4) [[1,0,0,1],[-3/2,2,3,3/2]]

testOrthogonal :: [Vector QQ] -> Bool
testOrthogonal [] = True
testOrthogonal (v:vs) = all (\w -> dot v w == 0) vs
                        && testOrthogonal vs
    where dot a b = getElem 1 1 $ a * transpose b