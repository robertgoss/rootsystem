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

class (RootSystem r rt) => SubRootSystems sr r rt | sr -> r, sr -> rt where
    commonAmbientSystem :: sr -> r
    subSystemsGenerators :: sr -> [[rt]]

data SubSystem a = SubS a CartanAlgebra

subSystem :: (SubRootSystem sr r rt) => sr -> SubSystem sr
subSystem sr = SubS sr alg 
    where alg = cartanAlgebra basicSys
          basicSys = fromRoots $ map coroot $ subGenerators sr

data BasicSubSystem sr r rt = SubsS sr r [rt]
data BasicSubSystems r rt = BasicSubs r [[rt]]

instance (RootSystem r rt) => SubRootSystem (BasicSubSystem sr r rt) r rt where
    ambientSystem (SubsS _ system _) = system
    subGenerators (SubsS _ _ gens) = gens

instance (RootSystem r rt) => SubRootSystems (BasicSubSystems r rt) r rt where
    commonAmbientSystem (BasicSubs r _) = r
    subSystemsGenerators (BasicSubs _ gens) = gens

subSystems :: (SubRootSystems sr r rt) => sr -> [BasicSubSystem sr r rt]
subSystems sr = map (SubsS sr amb) genslist
  where amb = commonAmbientSystem sr
        genslist = subSystemsGenerators sr

data CombinedSubs sr = ComSub [sr]

combineSubSystems :: (SubRootSystem sr r rt) => [sr] -> (CombinedSubs sr)
combineSubSystems = ComSub 

instance (SubRootSystem sr r rt) => SubRootSystems (CombinedSubs sr) r rt where
    commonAmbientSystem (ComSub srs) = ambientSystem $ head srs
    subSystemsGenerators (ComSub srs) = map subGenerators srs

instance (SubRootSystem sr r rt) => RootSystem (SubSystem sr) rt where
    cartanAlgebra (SubS _ alg) = alg 
    rank (SubS _ alg) = cartanRank alg
    generators (SubS sr _) = subGenerators sr

data Intersection sr rt = Inter sr [rt] CartanAlgebra


intersection :: (SubRootSystems sr r rt) => sr -> Intersection sr rt
intersection sr = Inter sr gens commonAlg
  where gens = simpleRoots allRootsSystem
        commonAlg = cartanAlgebra allRootsSystem
        allRootsSystem = fromRoots commonRoots
        commonRoots = Set.toList $ foldl1 Set.intersection $ map Set.fromList rootsList
        rootsList = map (roots . subSystem) subs
        subs = subSystems sr

instance (SubRootSystems sr r rt) => RootSystem (Intersection sr rt) rt where
    generators (Inter _ gens _) = gens
    cartanAlgebra (Inter _ _ alg) = alg
    rank (Inter _ _ alg) = cartanRank alg

