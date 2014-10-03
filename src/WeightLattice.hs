{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WeightLattice where

import RootSystem
import Generate
import qualified Data.Set as Set

import Data.Set as Set
import Data.Map as Map

newtype PrincipleWeight = PWeight [Int] deriving(Show)
data BasicLattice r rt = BasicLattice r (Map.Map rt PrincipleWeight)

instance Eq PrincipleWeight where
    (PWeight xs) == (PWeight ys) = xs' == ys'
        where m = length xs
              n = length ys
              xs' = xs ++ replicate (n-m) 0
              ys' = ys ++ replicate (m-n) 0


instance Ord PrincipleWeight where
    (PWeight xs) `compare` (PWeight ys) = xs' `compare` ys'
        where m = length xs
              n = length ys
              xs' = xs ++ replicate (n-m) 0
              ys' = ys ++ replicate (m-n) 0

negate :: PrincipleWeight -> PrincipleWeight
negate (PWeight xs) = PWeight $ Prelude.map (0-) xs

add :: PrincipleWeight -> PrincipleWeight -> PrincipleWeight
add (PWeight xs) (PWeight ys) = PWeight $ zipWith (+) xs' ys'
  where m = length xs
        n = length ys
        xs' = xs ++ replicate (n-m) 0
        ys' = ys ++ replicate (m-n) 0

basicWeight i = PWeight $ replicate (i-1) 0 ++ [1]

weightProduct :: PrincipleWeight -> PrincipleWeight -> Integer
weightProduct (PWeight xs) (PWeight ys)= fromIntegral . sum $ zipWith (*) xs ys

class (RootSystem r rt) => WeightLattice wl r rt | wl -> r, wl -> rt where
    generators :: wl -> [PrincipleWeight]
    innerProduct :: wl -> PrincipleWeight -> rt -> Integer
    associatedWeight :: wl -> rt -> PrincipleWeight
    underlyingSystem :: wl -> r


instance (RootSystem r rt) => WeightLattice (BasicLattice r rt) r rt where
    generators (BasicLattice r _) = Prelude.map basicWeight [1..nRoots]
        where nRoots = length $ simpleRoots r
    underlyingSystem (BasicLattice r _) = r
    associatedWeight (BasicLattice _ rMap) root = rMap ! root
    innerProduct (BasicLattice _ rMap) weight root = weightProduct weight $ rMap ! root

basicLattice :: (RootSystem r rt) => r -> BasicLattice r rt
basicLattice r = BasicLattice r rMap
    where rMap = Map.fromAscList pairs
          pairs = generateWithFailure comb $ zip (rGens++rGensN) (pGens++pGensN)
          rootSet = Set.fromList $ roots r
          rGens = simpleRoots r
          rGensN = Prelude.map RootSystem.negate rGens
          pGens = Prelude.map basicWeight [1..(length rGens)]
          pGensN = Prelude.map WeightLattice.negate pGens
          comb (r1,pw1) (r2,pw2) = case r1 `RootSystem.add` r2 of
                                      Nothing -> Nothing
                                      (Just rSum) -> if rSum `Set.member` rootSet then
                                                        Just (rSum, WeightLattice.add pw1 pw2)
                                                     else
                                                        Nothing
