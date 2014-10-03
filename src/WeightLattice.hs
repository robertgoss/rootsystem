{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module WeightLattice where

import RootSystem
import Generate

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

add :: PrincipleWeight -> PrincipleWeight -> PrincipleWeight
add (PWeight xs) (PWeight ys) = PWeight $ zipWith (+) xs' ys'
  where m = length xs
        n = length ys
        xs' = xs ++ replicate (n-m) 0
        ys' = ys ++ replicate (m-n) 0

class (RootSystem r rt) => WeightLattice wl r rt | wl -> r, wl -> rt where
    generators :: wl -> [PrincipleWeight]
    innerProduct :: wl -> PrincipleWeight -> rt -> Integer
    assosiatedWeight :: wl -> rt -> PrincipleWeight
    underlyingSystem :: wl -> r

  