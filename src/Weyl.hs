{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Weyl where

import Data.Matrix

import Rational
import RootSystem
import CartanAlgebra
import Generate
import SubSystem

class (Eq g,Root r) => WeylGroupElement g r | g -> r where

    inverse :: g -> g
    multiply :: g -> g -> g

    simpleReflection :: r -> g
    weylAction :: g -> r -> r

    torusRepresentation :: g -> BasicWeylGroupElement


class (RootSystem r rt, WeylGroupElement e rt) => WeylGroup w e r rt | w -> r, w -> e where
    one :: w -> e
    generators :: w -> [e]
    weylGroup :: r -> w


newtype BasicWeylGroupElement = BasicElement (Matrix QQ) deriving(Show)
data BasicWeylGroup = BasicGroup Int [BasicWeylGroupElement]

instance Eq BasicWeylGroupElement where
    (BasicElement m1) == (BasicElement m2) | d1 == d2 = m1 == m2
                                           | d1 < d2 = mpad (d2-d1) m1 == m2
                                           | otherwise = m1 == mpad (d1-d2) m2
        where d1 = nrows m1
              d2 = nrows m2
              mpad delta matrix = joinBlocks (matrix, zero size delta, zero delta size, identity delta)
                where size = nrows matrix

instance Ord BasicWeylGroupElement where
    (BasicElement m1) `compare` (BasicElement m2) | d1 == d2 = toList m1 `compare` toList m2
                                                  | d1 < d2 = toList (mpad (d2-d1) m1) `compare` toList m2
                                                  | otherwise = toList m1 `compare` toList (mpad (d1-d2) m2)
        where d1 = nrows m1
              d2 = nrows m2
              mpad delta matrix = joinBlocks (matrix, zero size delta, zero delta size, identity delta)
                where size = nrows matrix

reflectMatrix :: Vector QQ -> Matrix QQ
reflectMatrix vec = identity (ncols vec) - inv
    where inv = matrix (ncols vec) (ncols vec) invk
          invk (i,j) = 2 * getElem 1 i vec * getElem 1 j vec / len
          len = getElem 1 1 $ vec*transpose vec


instance WeylGroupElement BasicWeylGroupElement BasicRoot where

    inverse (BasicElement m) = BasicElement $ transpose m
    multiply (BasicElement m1) (BasicElement m2) | d1 == d2 = BasicElement $ m1*m2
                                                 | d1 < d2 = BasicElement $ mpad (d2-d1) m1 * m2
                                                 | otherwise = BasicElement $ m1 * mpad (d1-d2) m2
        where d1 = nrows m1
              d2 = nrows m2
              mpad delta matrix = joinBlocks (matrix, zero size delta, zero delta size, identity delta)
                where size = nrows matrix

    torusRepresentation = id

    simpleReflection (BasicRoot v) = BasicElement $ reflectMatrix v

    weylAction (BasicElement m) (BasicRoot r) = BasicRoot $ transpose $ mpad * transpose rpad
        where mDim = nrows m
              rDim = ncols r
              mpad = if mDim < rDim then
                        joinBlocks (m, zero mDim (rDim-mDim), zero (rDim-mDim) mDim, identity (rDim-mDim))
                     else
                        m
              rpad = r <|> zero 1 (mDim - rDim)

instance WeylGroup BasicWeylGroup BasicWeylGroupElement (BasicRootSystem BasicRoot) BasicRoot where

    one (BasicGroup dim gens) = BasicElement $ identity dim

    generators (BasicGroup _ gens) = gens
    weylGroup rootsystem = BasicGroup dim gens
        where gens = map simpleReflection $ RootSystem.generators rootsystem
              dim = ambientDim rootsystem


elements :: (WeylGroup w e r rt, Ord e) => w -> [e]
elements group = generate multiply (one group:gens)
    where gens = Weyl.generators group

order :: (WeylGroup w e r rt, Ord e) => w -> Integer
order = toInteger . length . elements

data ActedSubSystem sr g = ActSub g sr

instance (SubRootSystem sr r rt, WeylGroupElement e rt) => SubRootSystem (ActedSubSystem sr e) r rt where
  ambientSystem (ActSub _ sr) = ambientSystem sr
  subGenerators (ActSub g sr) = map (weylAction g) $ subGenerators sr
  subCartan (ActSub _ sr) = subCartan sr

actOnSubSytem :: (SubRootSystem sr r rt, WeylGroupElement e rt) => sr -> e -> ActedSubSystem sr e
actOnSubSytem = flip ActSub