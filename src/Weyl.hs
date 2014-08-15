{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Weyl where

import Data.Ratio
import Data.Matrix

import RootSystem

class WeylGroupElement g r | g -> r where

    one :: g
    inverse :: g -> g
    multiply :: g -> g -> g

    simpleReflection :: (Root r) => r -> g

    torusRepresentation :: g -> Matrix (Ratio Int)


class WeylGroup w e | w -> e where
    generators :: (WeylGroupElement e r) => w -> [e]
    weylGroup :: (WeylGroupElement e r,RootSystem s r,Root r) => s -> w


newtype BasicWeylGroupElement = BasicElement [Vector (Ratio Int)]
newtype BasicWeylGroup = BasicGroup [BasicWeylGroupElement]

instance WeylGroupElement BasicWeylGroupElement BasicRoot where
    one = BasicElement []
    inverse (BasicElement vs) = BasicElement $ reverse vs
    multiply (BasicElement vs) (BasicElement ws) = BasicElement $ vs++ws

    torusRepresentation (BasicElement vs) = product $ map basicReflect vs
        where basicReflect v = identity (nrows v) - inv
                           where inv = matrix (nrows v) (nrows v) invk
                                 invk (i,j) = 2 * getElem 1 i v * getElem 1 j v / len
                                 len = getElem 1 1 $ v*transpose v

    simpleReflection (BasicRoot v) = BasicElement $ [v]

instance WeylGroup BasicWeylGroup BasicWeylGroupElement where
    generators (BasicGroup gens) = gens
    weylGroup rootsystem = BasicGroup $ map simpleReflection $ RootSystem.generators rootsystem