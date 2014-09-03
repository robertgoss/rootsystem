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

class (Root r) => WeylGroupElement g r | g -> r where

    inverse :: g -> g
    multiply :: g -> g -> g

    simpleReflection :: r -> g

    torusRepresentation :: g -> Matrix QQ


class WeylGroup w where
    type RootType
    type ElementType
    type RootSystemType
    one :: w -> ElementType
    generators :: (WeylGroupElement ElementType RootType) => w -> [ElementType]
    weylGroup :: (RootSystem RootSystemType RootType) => RootSystemType -> w





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


instance WeylGroupElement BasicWeylGroupElement BasicRoot where

    inverse (BasicElement m) = BasicElement $ transpose m
    multiply (BasicElement m1) (BasicElement m2) = BasicElement $ m1*m2

    torusRepresentation (BasicElement m) = m

    simpleReflection (BasicRoot v) = BasicElement $ reflectMatrix v

instance WeylGroup BasicWeylGroup where
    type RootType = BasicRoot
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
