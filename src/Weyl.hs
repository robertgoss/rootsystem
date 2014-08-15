{-# LANGUAGE TypeFamilies #-}
module Weyl where

import Data.Ratio

class WeylGroup w where

    type RootSystem r
    rootElement :: r -> w
    weylAction :: w -> r -> r

    one :: w
    generators :: [w]
    inverse :: w -> w
    multiply :: w -> w -> w

    torusRepresentation :: w -> [[Ratio Int]]
