{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Quotient where

import Weyl
import SubGroup

class (WeylGroup w e r rt) => QuotientWeylGroup qw w e r rt | qw -> w where
    superGroup :: qw -> w
    quoWeylEq :: qw -> e -> e -> Bool

data SubGroupQuotient s w e = SubQuo s [e]

instance (WeylGroup w e r rt, SubWeylGroup s w e r rt) 
                => QuotientWeylGroup (SubGroupQuotient s w e) w e r rt where

    superGroup (SubQuo sub _) = ambientGroup sub

    quoWeylEq (SubQuo sub subElems) g1 g2 = difference `elem` subElems
                where difference = g1 `multiply` (inverse g2)