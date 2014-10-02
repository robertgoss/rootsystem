{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module SubSystem where

import qualified Data.Set as Set
import CartanAlgebra
import RootSystem

class (RootSystem r rt) => SubRootSystem sr r rt | sr -> r, sr -> rt where
    ambientSystem :: sr -> r
    subGenerators :: sr -> [rt] 
    subCartan :: sr -> CartanAlgebra

class (RootSystem r rt) => SubRootSystems sr r rt | sr -> r, sr -> rt where
    commonAmbientSystem :: sr -> r
    subSystemsCartan :: sr -> [CartanAlgebra]
    subSystemsGenerators :: sr -> [[rt]]


data SubSystem a = SubS a

subSystem :: (SubRootSystem sr r rt) => sr -> SubSystem sr
subSystem = SubS

data BasicSubSystem sr r rt = SubsS sr r [rt] CartanAlgebra
data BasicSubSystems r rt = BasicSubs r [[rt]] [CartanAlgebra]

instance (RootSystem r rt) => SubRootSystem (BasicSubSystem sr r rt) r rt where
    ambientSystem (SubsS _ system _ _) = system
    subGenerators (SubsS _ _ gens _) = gens
    subCartan (SubsS _ _ _ alg) = alg

instance (RootSystem r rt) => SubRootSystems (BasicSubSystems r rt) r rt where
    commonAmbientSystem (BasicSubs r _ _) = r
    subSystemsGenerators (BasicSubs _ gens _) = gens
    subSystemsCartan (BasicSubs _ _ alg) = alg

subSystems :: (SubRootSystems sr r rt) => sr -> [BasicSubSystem sr r rt]
subSystems sr = zipWith (SubsS sr amb) genslist algs
  where amb = commonAmbientSystem sr
        genslist = subSystemsGenerators sr
        algs = subSystemsCartan sr

data CombinedSubs sr sr' = ComSub sr sr'

combineSubSystems :: (SubRootSystem sr r rt,SubRootSystem sr' r rt) => sr -> sr' -> (CombinedSubs sr sr')
combineSubSystems = ComSub 

instance (SubRootSystem sr r rt,SubRootSystem sr' r rt) => SubRootSystems (CombinedSubs sr sr') r rt where
    commonAmbientSystem (ComSub sr _) = ambientSystem sr
    subSystemsGenerators (ComSub sr sr') = [subGenerators sr, subGenerators sr']
    subSystemsCartan (ComSub sr sr') = [subCartan sr, subCartan sr']

instance (SubRootSystem sr r rt) => RootSystem (SubSystem sr) rt where
    cartanAlgebra (SubS sr) = subCartan sr
    rank (SubS sr) = cartanRank $ subCartan sr
    generators (SubS sr) = subGenerators sr

data Intersection sr rt = Inter sr [rt] CartanAlgebra


intersection :: (SubRootSystems sr r rt) => sr -> Intersection sr rt
intersection sr = Inter sr gens commonAlg
  where gens = simpleRoots allRootsSystem
        commonAlg = foldl1 CartanAlgebra.intersect $ subSystemsCartan sr
        allRootsSystem = fromRoots commonRoots
        commonRoots = Set.toList $ foldl1 Set.intersection $ map Set.fromList rootsList
        rootsList = map (roots . subSystem) subs
        subs = subSystems sr

instance (SubRootSystems sr r rt) => RootSystem (Intersection sr rt) rt where
    generators (Inter _ gens _) = gens
    cartanAlgebra (Inter _ _ alg) = alg
    rank (Inter _ _ alg) = cartanRank alg

