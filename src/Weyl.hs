{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Weyl where

import Data.Matrix

import Rational
import RootSystem
import CartanAlgebra

class WeylGroupElement g where

    type WeylRootType

    one :: g
    inverse :: g -> g
    multiply :: g -> g -> g

    simpleReflection :: (Root WeylRootType) => WeylRootType -> g

    torusRepresentation :: g -> Matrix QQ


class WeylGroup w where
    type ElementType
    type RootSystemType
    generators :: (WeylGroupElement ElementType) => w -> [ElementType]
    weylGroup :: (RootSystem RootSystemType) => RootSystemType -> w


newtype BasicWeylGroupElement = BasicElement [Vector QQ]
newtype BasicWeylGroup = BasicGroup [BasicWeylGroupElement]

instance WeylGroupElement BasicWeylGroupElement where
    type WeylRootType = BasicRoot
    one = BasicElement []
    inverse (BasicElement vs) = BasicElement $ reverse vs
    multiply (BasicElement vs) (BasicElement ws) = BasicElement $ vs++ws

    torusRepresentation (BasicElement vs) = product $ map basicReflect vs
        where basicReflect v = identity (ncols v) - inv
                           where inv = matrix (ncols v) (ncols v) invk
                                 invk (i,j) = 2 * getElem 1 i v * getElem 1 j v / len
                                 len = getElem 1 1 $ v*transpose v

    simpleReflection (BasicRoot v) = BasicElement $ [v]

instance WeylGroup BasicWeylGroup where
    type ElementType = BasicWeylGroupElement
    type RootSystemType = BasicRootSystem
    generators (BasicGroup gens) = gens
    weylGroup rootsystem = BasicGroup $ map simpleReflection $ RootSystem.generators rootsystem