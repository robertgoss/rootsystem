module WeightLattice where

import RootSystem


newtype PrincipleWeight = PWeight [Int]
newtype WeightLattice r = WL r

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

zeroWeight = PWeight []

weightLattice :: (RootSystem r rt) => r -> WeightLattice r
weightLattice = WL

generators :: (RootSystem r rt) => WeightLattice r -> [PrincipleWeight]
generators (WL r) = map basicWeight [1..nRoots] 
    where nRoots = length $ simpleRoots r
          basicWeight i = PWeight $ replicate (i-1) 0 ++ [1] 

weylDimension :: (RootSystem r rt) => WeightLattice r -> PrincipleWeight -> Int
weylDimension = undefined

weightsByWeylDimension :: (RootSystem r rt) => WeightLattice r -> [PrincipleWeight]
weightsByWeylDimension = undefined