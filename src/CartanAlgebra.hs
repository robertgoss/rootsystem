module CartanAlgebra where

import Data.Matrix
import Data.Ratio

import Rational

type Vector a = Matrix a



newtype CartanAlgebra = CartanAlgebra [Vector QQ]

fullSubAlgebra :: Int -> CartanAlgebra
fullSubAlgebra n = CartanAlgebra $ map basis [1..n]
    where basis i = setElem 1 (1,i) $ zero 1 n

orthogonalBasis :: CartanAlgebra -> [Vector QQ]
orthogonalBasis (CartanAlgebra basis) = basis

span :: [Vector QQ] -> CartanAlgebra
span vectors = CartanAlgebra . reverse $ foldl gSmitt [] vectors
    where gSmitt partialBasis vector
            | dot reducedVector reducedVector == 0 = partialBasis
            | otherwise = reducedVector : partialBasis
            where reducedVector = foldl reduce vector partialBasis
                  reduce vec base = vec - scaleMatrix ((dot vec base) / (dot base base)) base
                  dot v w = getElem 1 1 $ v * transpose w

cartanRank :: CartanAlgebra -> Int
cartanRank = length . orthogonalBasis