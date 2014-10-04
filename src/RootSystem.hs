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
    coroot :: r -> BasicRoot
    add :: r -> r -> Maybe r
    positive :: r -> Bool
    negate :: r -> r
    lengthSq :: r -> Integer
    lengthSq = lengthSq . coroot

class (Root rt) => RootSystem r rt | r -> rt where
    generators :: (Root rt) => r -> [rt]
    generators = simpleRoots
    
    rank :: r -> Int
    cartanAlgebra :: r -> CartanAlgebra

    roots :: (RootSystem r rt) => r -> [rt]
    roots system = generate (flip reflect) $ generators system

    positiveRoots :: (RootSystem r rt) => r -> [rt]
    positiveRoots = filter positive . roots

    simpleRoots :: (RootSystem r rt) => r -> [rt]
    simpleRoots r = Set.toList $ Set.difference (Set.fromList positiveR) (Set.fromList sums)
        where positiveR = positiveRoots r
              sums = catMaybes $ [r `add` s | r<-positiveR, s<-positiveR, r > s]


rootsScan :: (RootSystem r rt) => r -> [([rt],[rt])]
rootsScan = generateScan (flip reflect) . generators

dim :: (RootSystem r rt) => r -> Int
dim system = rank system + length (roots system)

newtype BasicRoot = BasicRoot (Vector QQ) deriving (Show)
data BasicRootSystem r = BasicRootSystem CartanAlgebra [r]

instance Eq BasicRoot where
    (BasicRoot v1) == (BasicRoot v2) = v1padded == v2padded
      where n1 = ncols v1
            n2 = ncols v2
            k = max n1 n2
            v1padded = v1 <|> zero 1 (k - n1)
            v2padded = v2 <|> zero 1 (k - n2)

instance Ord BasicRoot where
    (BasicRoot v1) `compare` (BasicRoot v2) = getRow 1 v1padded `compare` getRow 1 v2padded
      where n1 = ncols v1
            n2 = ncols v2
            k = max n1 n2
            v1padded = v1 <|> zero 1 (k - n1)
            v2padded = v2 <|> zero 1 (k - n2)

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
    coroot = id
    (BasicRoot r) `add` (BasicRoot s) = Just . BasicRoot $ r' + s'
        where r' | ncols r >= ncols s = r
                 | otherwise = r <|> zero 1 (ncols s - ncols r)
              s' | ncols s >= ncols r = s
                 | otherwise = s <|> zero 1 (ncols r - ncols s)
    positive r = r > (BasicRoot $ zero 1 (basicDim r))
    negate (BasicRoot v) = BasicRoot $ scaleMatrix (-1) v
    lengthSq r = toInt $ dot r r


dot :: BasicRoot -> BasicRoot -> QQ
dot (BasicRoot v1) (BasicRoot v2) = sum $ zipWith (*) (toList v1) (toList v2)

isNonZero :: BasicRoot -> Bool
isNonZero root = dot root root /= 0

instance (Root r) => RootSystem (BasicRootSystem r) r where
    simpleRoots (BasicRootSystem _ roots) = roots
    rank (BasicRootSystem cartan _) = cartanRank cartan
    cartanAlgebra (BasicRootSystem cartan _) = cartan

data BasicGeneratorRootSystem r = BGRS [r] 

instance (Root rt) => RootSystem (BasicGeneratorRootSystem rt) rt where
  rank r = cartanRank $ cartanAlgebra r
  cartanAlgebra (BGRS gens) = CartanAlgebra.span $ map (vec . coroot) gens
    where vec (BasicRoot v) = v
  generators (BGRS gens) = gens
 
fromRoots :: (Root rt) => [rt] -> BasicRootSystem rt
fromRoots roots =  BasicRootSystem cartan sRoots
  where vec (BasicRoot v) = v
        cartan = CartanAlgebra.span $ map (vec . coroot) roots
        sRoots = simpleRoots (BGRS roots) 

torus :: CartanAlgebra -> BasicRootSystem r
torus cartan = BasicRootSystem cartan []

trivialSystem :: BasicRootSystem r
trivialSystem = torus $ trivialAlgebra

ambientDim (BasicRootSystem cartan _) | null basis = 0
                                      | otherwise = ncols $ head basis
                                      where basis = orthogonalBasis cartan

basicSystemProduct :: BasicRootSystem BasicRoot -> BasicRootSystem BasicRoot -> BasicRootSystem BasicRoot
basicSystemProduct system1 system2 = BasicRootSystem cartanProd rootProd
                where cartanProd = cartanProduct (cartanAlgebra system1) (cartanAlgebra system2)
                      rootProd = leftExtendRoots ++ rightExtendRoots
                      dim1 = ambientDim system1
                      dim2 = ambientDim system2
                      leftExtendRoots = map leftExtend $ generators system2
                      rightExtendRoots = map rightExtend $ generators system1
                      leftExtend (BasicRoot v) = BasicRoot $ zero 1 dim1 <|> v <|> zero 1 (dim2 - ncols v) 
                      rightExtend (BasicRoot v) = BasicRoot $ v <|> zero 1 (dim1 + dim2 - ncols v) 

canonicalRootSystem :: (RootSystem s rt, Root rt) => s -> BasicRootSystem BasicRoot
canonicalRootSystem rootsystem = BasicRootSystem cartan roots
    where   gens = generators rootsystem
            roots = map coroot gens
            cartan = cartanAlgebra rootsystem




instance Arbitrary BasicRoot where
    arbitrary = do a <- arbitrary
                   b <- arbitrary
                   c <- arbitrary
                   return (BasicRoot (fromList 1 3 [a,b,c]))
