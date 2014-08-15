module RootSystem where

import Data.Ratio

class RootSystem r where
    rank :: r -> Int
    generators :: [r]
    reflect :: r -> r -> r
    coroot :: r -> [Ratio Int]


class SubRootSystem s where
    injection :: (RootSystem r) => s -> r

