{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RootSystem where

import Data.Ratio
import Data.Matrix
import Test.SmallCheck.Series

import CartanAlgebra

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector (Ratio Int)

class RootSystem r where
    type RootType
    generators :: (Root RootType) => r -> [RootType]
    rank :: r -> Int
    cartanAlgebra :: r -> CartanAlgebra

newtype BasicRoot = BasicRoot (Vector (Ratio Int)) deriving (Eq,Show)
data BasicRootSystem = BasicRootSystem CartanAlgebra [BasicRoot]

instance Root BasicRoot where
    reflect (BasicRoot r) (BasicRoot s) = BasicRoot $ r - scaleMatrix (2*dot/len) s
        where dot = getElem 1 1 $ r*transpose s
              len = getElem 1 1 $ s*transpose s
    coroot (BasicRoot r) = r

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

instance (Monad m) => Serial m BasicRoot where
    series = cons4 root4
        where root4 a b c d = BasicRoot $ fromList 1 1 [a,b,c,d]