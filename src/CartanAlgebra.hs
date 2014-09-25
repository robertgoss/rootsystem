module CartanAlgebra where

import Data.Matrix
import Data.Ratio

import Rational

type Vector a = Matrix a



newtype CartanAlgebra = CartanAlgebra [Vector QQ]

fullSubAlgebra :: Int -> CartanAlgebra
fullSubAlgebra n = CartanAlgebra $ map basis [1..n]
    where basis i = setElem 1 (1,i) $ zero 1 n


trivialAlgebra :: CartanAlgebra
trivialAlgebra = CartanAlgebra []

orthogonalBasis :: CartanAlgebra -> [Vector QQ]
orthogonalBasis (CartanAlgebra basis) = basis

span' :: [Vector QQ] -> CartanAlgebra
span' vectors = CartanAlgebra . reverse $ foldl gSmitt [] vectors
    where gSmitt partialBasis vector
            | dot reducedVector reducedVector == 0 = partialBasis
            | otherwise = reducedVector : partialBasis
            where reducedVector = foldl reduce vector partialBasis
                  reduce vec base = vec - scaleMatrix ((dot vec base) / (dot base base)) base
                  dot v w = getElem 1 1 $ v * transpose w

span :: [Vector QQ] -> CartanAlgebra
span vectors = span' $ map pad vectors
  where dim = maximum $ map ncols vectors
        pad v = v <|> zero 1 (dim - ncols v)

cartanRank :: CartanAlgebra -> Int
cartanRank = length . orthogonalBasis


cartanProduct :: CartanAlgebra -> CartanAlgebra -> CartanAlgebra
cartanProduct cartan (CartanAlgebra []) = cartan
cartanProduct (CartanAlgebra []) cartan = cartan
cartanProduct (CartanAlgebra basis1) (CartanAlgebra basis2) = CartanAlgebra (leftExtend++rightExtend)
    where leftPad = zero 1 . ncols . head $ basis1
          rightPad = zero 1 . ncols . head $ basis2
          leftExtend = map (<|> rightPad) basis1
          rightExtend = map (leftPad <|>) basis2