module Test.TestCartanAlgebra where

import Test.Tasty
import Test.Tasty.SmallCheck as SC
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Data.Matrix
import Data.Ratio

import RootSystem
import CartanAlgebra

testOrthogonalBasis = testGroup "" [testOrthogonalBasisProp, testOrthogonalBasisUnit]

testOrthogonalBasisProp = testGroup "Properties" [testOrthogonalality, testOrthIdopotent, testOrthRepeat]
testOrthogonalBasisUnit = testGroup "Unit tests" []

makeBasis :: [Vector (Ratio Integer)] -> [Vector (Ratio Integer)]
makeBasis vecs = orthogonalBasis . CartanAlgebra.span $ vecs

testOrthogonalality = QC.testProperty "Orthogonal Basis should be orthogonal" $ \roots ->
        testOrthogonal . makeBasis . (map coroot) $ (roots::[BasicRoot])

testOrthIdopotent = QC.testProperty "Orthogonal basis should be ideopotent" $ \roots ->
        (makeBasis . makeBasis) (map coroot (roots::[BasicRoot])) == makeBasis (map coroot roots)

testOrthRepeat = QC.testProperty "Orthogonal basis should be reduce a repeated root" $ \root ->
        makeBasis (map coroot [root,root,root]) == [coroot (root::BasicRoot)]

testOrthogonal :: [Vector (Ratio Integer)] -> Bool
testOrthogonal [] = True
testOrthogonal (v:vs) = all (\w -> dot v w == 0) vs
                        && testOrthogonal vs
    where dot a b = getElem 1 1 $ a * transpose b