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


class (RootSystem r rt, WeylGroupElement e rt) => WeylGroup w e r rt | w -> r, w -> e where
    one :: w -> e
    generators :: w -> [e]
    weylGroup :: r -> w


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
    multiply (BasicElement m1) (BasicElement m2) | d1 == d2 = BasicElement $ m1*m2
                                                 | d1 < d2 = BasicElement $ (mpad (d2-d1) m1) * m2
                                                 | otherwise = BasicElement $ m1 * (mpad (d1-d2) m2)
        where d1 = nrows m1
              d2 = nrows m2
              mpad delta matrix = joinBlocks (matrix, zero delta size, zero size delta, identity delta)
                where size = nrows matrix

    torusRepresentation (BasicElement m) = m

    simpleReflection (BasicRoot v) = BasicElement $ reflectMatrix v

instance WeylGroup BasicWeylGroup BasicWeylGroupElement BasicRootSystem BasicRoot where

    one (BasicGroup dim gens) = BasicElement $ identity dim

    generators (BasicGroup _ gens) = gens
    weylGroup rootsystem = BasicGroup dim gens
        where gens = map simpleReflection $ RootSystem.generators rootsystem
              dim | null (RootSystem.generators rootsystem) = 1
                  | otherwise = ncols . coroot . head . RootSystem.generators $ rootsystem


elements :: (WeylGroup w e r rt,Ord e) => w -> [e]
elements group = generate multiply (one group:gens)
    where gens = Weyl.generators group

order :: (WeylGroup w e r rt, Ord e) => w -> Integer
order = toInteger . length . elements


basicElement :: (WeylGroupElement e r) => e -> BasicWeylGroupElement
basicElement = BasicElement . torusRepresentation