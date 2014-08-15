{-# LANGUAGE TypeFamilies #-}
module Weyl where

import Data.Ratio
import Data.Matrix

import RootSystem



class WeylGroupElement g where

    one :: g
    inverse :: g -> g
    multiply :: g -> g -> g

    torusRepresentation :: g -> Matrix (Ratio Int)


class WeylGroup w where
    type WeylElems e
    type WeylSystem r
    generators :: (WeylGroupElement e) => w -> [WeylElems e]
    rootSystem :: (RootSystem r) => w -> WeylSystem r
    weylReflection :: (RootSystem r, WeylGroupElement e) => w -> (RootType (WeylSystem r)) -> (WeylElems e)
    weylAction :: (RootSystem r, WeylGroupElement e) => w -> (WeylElems e) -> (RootType (WeylSystem r)) -> (RootType (WeylSystem r))



