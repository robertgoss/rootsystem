{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SubGroup where

import RootSystem
import Weyl


class (WeylGroup w e r rt) => SubWeylGroup qw w e r rt | qw -> w, qw -> e where
    ambientGroup :: qw -> w
    subGenerators :: qw -> [e]

