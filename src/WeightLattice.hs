module WeightLattice where

import RootSystem


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
  where pWeights = map (associatedPWeight lattice) $ positiveRoots r
        rho = foldl WeightLattice.add zeroWeight pWeights
        numerator = sum $ map (\pw -> WeightLattice.add weight rho `innerProduct` pw) pWeights
        denominator = sum $ map (\pw -> rho `innerProduct` pw) pWeights

associatedPWeight :: (RootSystem r rt) => WeightLattice r -> rt -> PrincipleWeight
associatedPWeight lattice@(WL r) root | not (positive root) =  associatedPWeight lattice (RootSystem.negate root)
                                      | otherwise = associatedPWeight' rGens pGens root
  where rGens = simpleRoots r
        pGens = WeightLattice.generators lattice

associatedPWeight' rGens@(rGen:rRest) pGens@(pGen:pRest) root
           | root == rGen = pGen
           | otherwise = case (sub rGen root) of
                            Nothing -> associatedPWeight' rRest pRest root
                            (Just root') ->  WeightLattice.add pGen $ associatedPWeight' rGens pGens root'
    where sub r1 r2 = RootSystem.add r2 (RootSystem.negate r1)

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