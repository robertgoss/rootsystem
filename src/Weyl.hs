{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Weyl where

import Data.Matrix

import Rational
import RootSystem
import CartanAlgebra
import Generate

class WeylGroupElement g where

    type WeylRootType

    inverse :: g -> g
    multiply :: g -> g -> g

    simpleReflection :: (Root WeylRootType) => WeylRootType -> g

    torusRepresentation :: g -> Matrix QQ


class WeylGroup w where
    type ElementType
    type RootSystemType
    one :: w -> ElementType
    generators :: (WeylGroupElement ElementType) => w -> [ElementType]
    weylGroup :: (RootSystem RootSystemType) => RootSystemType -> w

class SubWeylGroup s where
    type GroupType
    type SubSystemType
    superGroup :: (WeylGroup GroupType) => s -> GroupType
    subGroup :: s -> GroupType
    generatorImages :: s -> [ElementType]
    weylSubSystem :: (SubSystem SubSystemType) =>SubSystemType -> s

class SubWeylGroup2 s where
    type GroupType2
    type SubSystemType2
    dualSuperGroup :: (WeylGroup GroupType2) => s -> GroupType2
    subGroup1 :: s -> GroupType2
    generatorImages1 :: s -> [ElementType]
    subGroup2 :: s -> GroupType2
    generatorImages2 :: s -> [ElementType]
    weylSubSystem2 :: (SubSystem2 SubSystemType2) => SubSystemType2 -> s



newtype BasicWeylGroupElement = BasicElement (Matrix QQ) deriving(Eq)
data BasicWeylGroup = BasicGroup Int [BasicWeylGroupElement]
data BasicWeylSubGroup = BasicWeylSub BasicWeylGroup BasicWeylGroup [BasicWeylGroupElement]
data BasicWeylSubGroup2 = BasicWeylSub2 BasicWeylGroup BasicWeylGroup BasicWeylGroup [BasicWeylGroupElement] [BasicWeylGroupElement]

instance Ord BasicWeylGroupElement where
    (BasicElement m1) `compare` (BasicElement m2) = (toList m1) `compare` (toList m2)

reflectMatrix :: Vector QQ -> Matrix QQ
reflectMatrix vec = identity (ncols vec) - inv
    where inv = matrix (ncols vec) (ncols vec) invk
          invk (i,j) = 2 * getElem 1 i vec * getElem 1 j vec / len
          len = getElem 1 1 $ vec*transpose vec


instance WeylGroupElement BasicWeylGroupElement where
    type WeylRootType = BasicRoot

    inverse (BasicElement m) = BasicElement $ transpose m
    multiply (BasicElement m1) (BasicElement m2) = BasicElement $ m1*m2

    torusRepresentation (BasicElement m) = m

    simpleReflection (BasicRoot v) = BasicElement $ reflectMatrix v

instance WeylGroup BasicWeylGroup where
    type ElementType = BasicWeylGroupElement
    type RootSystemType = BasicRootSystem
    one (BasicGroup dim gens) = BasicElement $ identity dim

    generators (BasicGroup _ gens) = gens
    weylGroup rootsystem = BasicGroup dim gens
        where gens = map simpleReflection $ RootSystem.generators rootsystem
              dim | null (RootSystem.generators rootsystem) = 1
                  | otherwise = ncols . coroot . head . RootSystem.generators $ rootsystem


elements :: (WeylGroup w,Ord ElementType) => w -> [ElementType]
elements group = generate multiply (one group:gens)
    where gens = Weyl.generators group

order :: (WeylGroup w, Ord ElementType) => w -> Integer
order = toInteger . length . elements

instance SubWeylGroup BasicWeylSubGroup where
    type GroupType = BasicWeylGroup
    type SubSystemType = BasicSubSystem
    superGroup (BasicWeylSub super _ _) = super
    subGroup (BasicWeylSub _ sub _) = sub
    generatorImages (BasicWeylSub _ _ gens) = gens
    weylSubSystem (BasicSubSystem super sub gens) = BasicWeylSub wSuper wSub wGens
        where wSuper = weylGroup super
              wSub = weylGroup sub
              wGens = map simpleReflection gens

instance SubWeylGroup2 BasicWeylSubGroup2 where
    type GroupType2 = BasicWeylGroup
    type SubSystemType2 = BasicSubSystem2
    dualSuperGroup (BasicWeylSub2 super _ _ _ _) = super
    subGroup1 (BasicWeylSub2 _ sub1 _ _ _) = sub1
    generatorImages1 (BasicWeylSub2 _ _ _ gens1 _) = gens1
    subGroup2 (BasicWeylSub2 _ _ sub2 _ _) = sub2
    generatorImages2 (BasicWeylSub2 _ _ _ _ gens2) = gens2
    weylSubSystem2 (BasicSubSystem2 super sub1 sub2 gens1 gens2) = BasicWeylSub2 wSuper wSub1 wSub2 wGens1 wGens2
        where wSuper = weylGroup super
              wSub1 = weylGroup sub1
              wSub2 = weylGroup sub2
              wGens1 = map simpleReflection gens1
              wGens2 = map simpleReflection gens2
