{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module RootSystem where

import Data.Ratio
import Data.Matrix

type Vector a = Matrix a

class Root r where
    reflect :: r -> r -> r
    coroot :: r -> Vector (Ratio Int)

class RootSystem r where
    type RootType
    generators :: (Root RootType) => r -> [RootType]
    rank :: r -> Int

newtype BasicRoot = BasicRoot (Vector (Ratio Int))
newtype BasicRootSystem = BasicRootSystem [BasicRoot]

instance Root BasicRoot where
    reflect (BasicRoot r) (BasicRoot s) = BasicRoot $ r - scaleMatrix (2*dot/len) s
        where dot = getElem 1 1 $ r*transpose s
              len = getElem 1 1 $ s*transpose s
    coroot (BasicRoot r) = r

instance RootSystem BasicRootSystem where
    type RootType = BasicRoot
    generators (BasicRootSystem roots) = roots
    rank (BasicRootSystem roots) = nrows $ coroot (head roots)


basicDim (BasicRootSystem []) = 0
basicDim (BasicRootSystem roots) = nrows $ coroot (head roots)

canonicalRootSystem :: (RootSystem s,Root RootType) => s -> BasicRootSystem
canonicalRootSystem rootsystem = BasicRootSystem roots
    where   gens = generators rootsystem :: [RootType]
            roots = map (BasicRoot . coroot) gens