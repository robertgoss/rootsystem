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
import Data.Maybe(catMaybes)

import           CartanAlgebra
import           Rational
import           Generate

class (Ord r) => Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector QQ
    add :: r -> r -> Maybe r
    positive :: r -> Bool
    negate :: r -> r

class (Root rt) => RootSystem r rt | r -> rt where
    generators :: (Root rt) => r -> [rt]
    rank :: r -> Int
    cartanAlgebra :: r -> CartanAlgebra


rootsScan :: (RootSystem r rt) => r -> [([rt],[rt])]
rootsScan = generateScan (flip reflect) . generators

roots :: (RootSystem r rt) => r -> [rt]
roots system = generate (flip reflect) $ generators system

positiveRoots :: (RootSystem r rt) => r -> [rt]
positiveRoots = filter positive . roots

simpleRoots :: (RootSystem r rt) => r -> [rt]
simpleRoots r = Set.toList $ Set.difference (Set.fromList positiveR) (Set.fromList sums)
    where positiveR = positiveRoots r
          sums = catMaybes $ [r `add` s | r<-positiveR, s<-positiveR, r > s]

dim :: (RootSystem r rt) => r -> Int
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
    reflect (BasicRoot r) (BasicRoot s) = BasicRoot $ r' - scaleMatrix (2*dot/len) s'
        where dot = getElem 1 1 $ r'*transpose s'
              len = getElem 1 1 $ s'*transpose s'
              r' | ncols r >= ncols s = r
                 | otherwise = r <|> zero 1 (ncols s - ncols r)
              s' | ncols s >= ncols r = s
                 | otherwise = s <|> zero 1 (ncols r - ncols s)
    coroot (BasicRoot r) = r
    (BasicRoot r) `add` (BasicRoot s) = Just . BasicRoot $ r + s
    positive r = r > (BasicRoot $ zero 1 (basicDim r))
    negate (BasicRoot v) = BasicRoot $ scaleMatrix (-1) v

toBasic :: (Root r) => r-> BasicRoot
toBasic = BasicRoot . coroot

toBasics :: (Root r) => [r]-> [BasicRoot]
toBasics roots = map BasicRoot paddedCoroots
    where coroots = map coroot roots
          dim = maximum $ map ncols coroots
          paddedCoroots = map (\v->v <|> zero 1 (dim - ncols v)) coroots

dot :: BasicRoot -> BasicRoot -> QQ
dot (BasicRoot v1) (BasicRoot v2) = getElem 1 1 $ v1 * transpose v2

isNonZero :: BasicRoot -> Bool
isNonZero root = (dot root root) /= 0

instance RootSystem BasicRootSystem BasicRoot where
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

canonicalRootSystem :: (RootSystem s rt, Root rt) => s -> BasicRootSystem
canonicalRootSystem rootsystem = BasicRootSystem cartan roots
    where   gens = (generators rootsystem)
            roots = map (BasicRoot . coroot) gens
            cartan = cartanAlgebra rootsystem




instance Arbitrary BasicRoot where
    arbitrary = do a <- arbitrary
                   b <- arbitrary
                   c <- arbitrary
                   return (BasicRoot (fromList 1 3 [a,b,c]))
