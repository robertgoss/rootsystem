module WeightLattice where

import RootSystem
import Generate
import qualified Data.Set as Set

newtype PrincipleWeight = PWeight [Int] deriving(Show)
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

negate :: PrincipleWeight -> PrincipleWeight
negate (PWeight xs) = PWeight $ map (0-) xs

add :: PrincipleWeight -> PrincipleWeight -> PrincipleWeight
add (PWeight xs) (PWeight ys) = PWeight $ zipWith (+) xs' ys'
  where m = length xs
        n = length ys
        xs' = xs ++ replicate (n-m) 0
        ys' = ys ++ replicate (m-n) 0

innerProduct :: PrincipleWeight -> PrincipleWeight -> Int
innerProduct (PWeight xs) (PWeight ys) = sum $ zipWith (*) xs ys

zeroWeight = PWeight []

weightLattice :: (RootSystem r rt) => r -> WeightLattice r
weightLattice = WL

generators :: (RootSystem r rt) => WeightLattice r -> [PrincipleWeight]
generators (WL r) = map basicWeight [1..nRoots] 
    where nRoots = length $ simpleRoots r
          basicWeight i = PWeight $ replicate (i-1) 0 ++ [1] 

weylDimension :: (RootSystem r rt) => WeightLattice r -> PrincipleWeight -> Int
weylDimension lattice@(WL r) weight = numerator `div` denominator
    where rootWeights' = rootWeights lattice
          rho = foldl (WeightLattice.add) zeroWeight rootWeights'
          numerator = sum $ map (\pw -> WeightLattice.add weight rho `innerProduct` pw) rootWeights'
          denominator = sum $ map (\pw -> rho `innerProduct` pw) rootWeights'

rootWeights :: (RootSystem r rt) => WeightLattice r -> [PrincipleWeight]
rootWeights lattice@(WL r) = map snd $ filter (positive . fst) $ generateWithFailure comb $ zip (rGens++rGensN) (pGens++pGensN)
    where rGens = simpleRoots r
          rGensN = map RootSystem.negate rGens
          pGens = WeightLattice.generators lattice
          pGensN = map WeightLattice.negate pGens
          rootsSet = Set.fromList $ roots r
          comb (r1,pw1) (r2,pw2) = case RootSystem.add r1 r2 of
                                       Nothing -> Nothing
                                       (Just rSum) -> if rSum `Set.member` rootsSet then
                                                          Just (rSum, WeightLattice.add pw1 pw2)
                                                      else
                                                          Nothing

pWeightsRestrictedWeylDimension :: (RootSystem r rt) => WeightLattice r -> Int -> [PrincipleWeight]
pWeightsRestrictedWeylDimension lattice n = pWeightsRestrictedWeylDimension' lattice n zeroWeight gens
 where gens = WeightLattice.generators lattice

pWeightsRestrictedWeylDimension' lattice n base [] = [base]
pWeightsRestrictedWeylDimension' lattice n base (gen:gens) = concatMap rebaseRestrict validWeights
    where validWeights = takeWhile validDim $ iterate (WeightLattice.add gen) base
          validDim pWeight = weylDimension lattice pWeight <= n
          rebaseRestrict pWeight = pWeightsRestrictedWeylDimension' lattice n pWeight gens

pWeightsWithWeylDimension :: (RootSystem r rt) => WeightLattice r -> Int -> [PrincipleWeight]
pWeightsWithWeylDimension lattice n = filter (\pw->n == weylDimension lattice pw) $ pweights
  where pweights = pWeightsRestrictedWeylDimension lattice n