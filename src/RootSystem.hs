{-# LANGUAGE TypeFamilies #-}
module RootSystem where

import Data.Ratio

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> [Ratio Int]

class RootSystem r where
    type RootType r
    generators :: r -> [RootType r]
    rank :: r -> Int

