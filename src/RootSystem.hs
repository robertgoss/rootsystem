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
import qualified Data.Set as Set (toList,fromList,difference)

import           CartanAlgebra
import           Rational
import           Generate

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector QQ
    add :: r -> r -> r
    positive :: r -> Bool

class RootSystem r where
    type RootType
    generators :: (Root RootType) => r -> [RootType]
    rank :: r -> Int
    cartanAlgebra :: r -> CartanAlgebra

class SubSystem s where
    type SystemType
    superSystem :: (RootSystem SystemType) => s -> SystemType
    generatorImages :: (Root RootType) => s -> [RootType]
    subSystem :: (RootSystem SystemType) => s -> SystemType

class SubSystem2 s where
    type SystemType2
    dualSuperSystem :: (RootSystem SystemType) => s -> SystemType
    generatorImages1 :: (Root RootType) => s -> [RootType]
    subSystem1 :: (RootSystem SystemType) => s -> SystemType
    generatorImages2 :: (Root RootType) => s -> [RootType]
    subSystem2 :: (RootSystem SystemType) => s -> SystemType

rootsScan :: (RootSystem r) => r -> [([RootType],[RootType])]
rootsScan = generateScan (flip reflect) . generators

roots :: (RootSystem r) => r -> [RootType]
roots system = generate (flip reflect) $ generators system

positiveRoots :: (RootSystem r) => r -> [RootType]
positiveRoots = filter positive . roots

simpleRoots :: (RootSystem r) => r -> [RootType]
simpleRoots r = Set.toList $ Set.difference (Set.fromList positiveR) (Set.fromList sums)
    where positiveR = positiveRoots r
          sums = [r `add` s | r<-positiveR, s<-positiveR, r > s]

dim :: (RootSystem r) => r -> Int
dim system = rank system + length (roots system)

newtype BasicRoot = BasicRoot (Vector QQ) deriving (Eq,Show)
data BasicRootSystem = BasicRootSystem CartanAlgebra [BasicRoot]
data BasicSubSystem = BasicSubSystem BasicRootSystem BasicRootSystem [BasicRoot]
data BasicSubSystem2 = BasicSubSystem2 BasicRootSystem BasicRootSystem BasicRootSystem [BasicRoot] [BasicRoot]

instance Ord BasicRoot where
    (BasicRoot v1) `compare` (BasicRoot v2) = (getRow 1 v1) `compare` (getRow 1 v2)

basicDim :: BasicRoot -> Int
basicDim (BasicRoot r) = ncols r

instance Root BasicRoot where
    reflect (BasicRoot r) (BasicRoot s) = BasicRoot $ r - scaleMatrix (2*dot/len) s
        where dot = getElem 1 1 $ r*transpose s
              len = getElem 1 1 $ s*transpose s
    coroot (BasicRoot r) = r
    (BasicRoot r) `add` (BasicRoot s) = BasicRoot $ r + s
    positive r = r > (BasicRoot $ zero 1 (basicDim r))

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


instance SubSystem BasicSubSystem where
    type SystemType = BasicRootSystem
    superSystem (BasicSubSystem super _ _) = super
    generatorImages (BasicSubSystem _ _ gens) = gens
    subSystem (BasicSubSystem _ sub _) = sub

fullSubSystem :: BasicRootSystem -> [BasicRoot] -> BasicSubSystem
fullSubSystem super gens = BasicSubSystem super sub gens
    where sub = fromRoots gens

instance SubSystem2 BasicSubSystem2 where
    type SystemType2 = BasicRootSystem
    dualSuperSystem (BasicSubSystem2 super _ _ _ _) = super
    generatorImages1 (BasicSubSystem2 _ _ _ _ gens2) = gens2
    subSystem1 (BasicSubSystem2 _ sub1 _ _ _) = sub1
    generatorImages2(BasicSubSystem2 _ _ _ gens1 _) = gens1
    subSystem2 (BasicSubSystem2 _ _ sub2 _ _) = sub2

fullSubSystem2 :: BasicRootSystem -> [BasicRoot] -> [BasicRoot] -> BasicSubSystem2
fullSubSystem2 super gens1 gens2 = BasicSubSystem2 super sub1 sub2 gens1 gens2
    where sub1 = fromRoots gens1
          sub2 = fromRoots gens2


instance Arbitrary BasicRoot where
    arbitrary = do a <- arbitrary
                   b <- arbitrary
                   c <- arbitrary
                   return (BasicRoot (fromList 1 3 [a,b,c]))
