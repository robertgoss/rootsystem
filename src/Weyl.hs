{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Weyl where

import Data.Ratio
import Data.Matrix

import RootSystem



class WeylGroupElement g where

    one :: g
    inverse :: g -> g
    multiply :: g -> g -> g

    torusRepresentation :: g -> Matrix (Ratio Int)


class WeylGroup w e r s | w -> e, w -> r, r -> s where
    generators :: (WeylGroupElement e) => w -> e
    rootSystem :: (RootSystem r s) => w -> r
    weylReflection :: (RootSystem r s, WeylGroupElement e) => w -> s -> e
    weylAction :: (RootSystem r s, WeylGroupElement e) => w -> e -> s -> s



