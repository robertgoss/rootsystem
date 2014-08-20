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


newtype BasicWeylGroupElement = BasicElement (Matrix QQ) deriving(Eq)
newtype BasicWeylGroup = BasicGroup [BasicWeylGroupElement]

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
    one (BasicGroup gens) = BasicElement $ identity . nrows . torusRepresentation . head $ gens

    generators (BasicGroup gens) = gens
    weylGroup rootsystem = BasicGroup $ map simpleReflection $ RootSystem.generators rootsystem


elements :: (WeylGroup w,Ord ElementType) => w -> [ElementType]
elements group = generate multiply gens
    where gens = Weyl.generators group

order :: (WeylGroup w, Ord ElementType) => w -> Integer
order = toInteger . length . elements 