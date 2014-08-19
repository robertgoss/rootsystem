{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}
module RootSystem where

import           Data.Matrix
import           Data.Ratio
import qualified Data.Vector               as V
import           Test.QuickCheck.Arbitrary

import           CartanAlgebra
import           Rational

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector QQ

class RootSystem r where
    type RootType
    generators :: (Root RootType) => r -> [RootType]
    rank :: r -> Int
    cartanAlgebra :: r -> CartanAlgebra

newtype BasicRoot = BasicRoot (Vector QQ) deriving (Eq,Show)
data BasicRootSystem = BasicRootSystem CartanAlgebra [BasicRoot]

instance Root BasicRoot where
    reflect (BasicRoot r) (BasicRoot s) = BasicRoot $ r - scaleMatrix (2*dot/len) s
        where dot = getElem 1 1 $ r*transpose s
              len = getElem 1 1 $ s*transpose s
    coroot (BasicRoot r) = r

isNonZero :: BasicRoot -> Bool
isNonZero (BasicRoot vec) = V.any (/=0) (getCol 1 vec)

instance RootSystem BasicRootSystem where
    type RootType = BasicRoot
    generators (BasicRootSystem _ roots) = roots
    rank (BasicRootSystem cartan _) = cartanRank cartan
    cartanAlgebra (BasicRootSystem cartan _) = cartan

fromRoots :: [BasicRoot] -> BasicRootSystem
fromRoots roots = BasicRootSystem (CartanAlgebra.span (map coroot roots)) roots

torus :: CartanAlgebra -> BasicRootSystem
torus cartan = BasicRootSystem cartan []

basicDim (BasicRootSystem cartan _) | null basis = 0
                                    | otherwise = nrows $ head basis
                                    where basis = orthogonalBasis cartan

canonicalRootSystem :: (RootSystem s,Root RootType) => s -> BasicRootSystem
canonicalRootSystem rootsystem = BasicRootSystem cartan roots
    where   gens = generators rootsystem :: [RootType]
            roots = map (BasicRoot . coroot) gens
            cartan = cartanAlgebra rootsystem

instance Arbitrary BasicRoot where
    arbitrary = do a <- arbitrary
                   b <- arbitrary
                   c <- arbitrary
                   return (BasicRoot (fromList 1 3 [a,b,c]))
