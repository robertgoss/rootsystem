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
import           Generate

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector QQ

class RootSystem r where
    type RootType
    generators :: (Root RootType) => r -> [RootType]
    rank :: r -> Int
    cartanAlgebra :: r -> CartanAlgebra

rootsScan :: (RootSystem r) => r -> [([RootType],[RootType])]
rootsScan = generateScan (flip reflect) . generators

roots :: (RootSystem r) => r -> [RootType]
roots system = generate (flip reflect) $ generators system

dim :: (RootSystem r) => r -> Int
dim system = rank system + length (roots system)

newtype BasicRoot = BasicRoot (Vector QQ) deriving (Eq,Show)
data BasicRootSystem = BasicRootSystem CartanAlgebra [BasicRoot]

instance Ord BasicRoot where
    (BasicRoot v1) `compare` (BasicRoot v2) = (getRow 1 v1) `compare` (getRow 1 v2)

instance Root BasicRoot where
    reflect (BasicRoot r) (BasicRoot s) = BasicRoot $ r - scaleMatrix (2*dot/len) s
        where dot = getElem 1 1 $ r*transpose s
              len = getElem 1 1 $ s*transpose s
    coroot (BasicRoot r) = r

dot :: BasicRoot -> BasicRoot -> QQ
dot (BasicRoot v1) (BasicRoot v2) = getElem 1 1 $ v1 * transpose v2

isNonZero :: BasicRoot -> Bool
isNonZero root = (dot root root) /= 0

instance RootSystem BasicRootSystem where
    type RootType = BasicRoot
    generators (BasicRootSystem _ roots) = roots
    rank (BasicRootSystem cartan _) = cartanRank cartan
    cartanAlgebra (BasicRootSystem cartan _) = cartan

fromRoots :: [BasicRoot] -> BasicRootSystem
fromRoots roots = BasicRootSystem (CartanAlgebra.span (map coroot roots)) roots

torus :: CartanAlgebra -> BasicRootSystem
torus cartan = BasicRootSystem cartan []

trivialSystem :: BasicRootSystem
trivialSystem = torus $ trivialAlgebra

ambientDim (BasicRootSystem cartan _) | null basis = 0
                                      | otherwise = ncols $ head basis
                                      where basis = orthogonalBasis cartan

basicSystemProduct :: BasicRootSystem -> BasicRootSystem -> BasicRootSystem
basicSystemProduct system1 system2 = BasicRootSystem cartanProd rootProd
                where cartanProd = cartanProduct (cartanAlgebra system1) (cartanAlgebra system2)
                      rootProd = leftExtendRoots ++ rightExtendRoots
                      leftPad = zero 1 $ ambientDim system1
                      rightPad = zero 1 $ ambientDim system2
                      leftExtendRoots = map ( BasicRoot . (<|> rightPad) . coroot ) $ generators system1
                      rightExtendRoots = map (BasicRoot . (leftPad <|>) . coroot ) $ generators system2

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
