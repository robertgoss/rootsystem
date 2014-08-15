{-# LANGUAGE TypeFamilies #-}
module RootSystem where

import Data.Ratio
import Data.Matrix

type Vector a = Matrix a

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector (Ratio Int)

class RootSystem r where
    type RootType r
    generators :: r -> [RootType r]
    rank :: r -> Int

