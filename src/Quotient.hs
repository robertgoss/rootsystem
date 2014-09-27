{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Quotient where

import Weyl
import SubGroup
import Generate



class (WeylGroup w e r rt) => QuotientWeylGroup qw w e r rt | qw -> w where
    superGroup :: qw -> w
    quoWeylEq :: qw -> e -> e -> Bool

data SubGroupQuotient s w e = SubQuo s [e]

instance (WeylGroup w e r rt, SubWeylGroup s w e r rt) 
                => QuotientWeylGroup (SubGroupQuotient s w e) w e r rt where

    superGroup (SubQuo sub _) = ambientGroup sub

    quoWeylEq (SubQuo sub subElems) g1 g2 = difference `elem` subElems
                where difference = g1 `multiply` (inverse g2)

data Quo a = Quo a (a->a->Bool)

instance Eq (Quo a) where
    (Quo x eqFun) == (Quo y _) = eqFun x y

preimage (Quo a _) = a

pushforward2 :: (a -> a -> a) -> Quo a -> Quo a -> Quo a
pushforward2 f (Quo a eqFun) (Quo b _)= Quo (f a b) eqFun 

repElements :: QuotientWeylGroup qw w e r rt => qw -> [e]
repElements quotientGroup = map preimage $ generateUnOrd multQuo quoGens
    where super = superGroup quotientGroup
          superGens = generators super
          multQuo = pushforward2 multiply
          quoGens = map (\g->Quo g quoEq) superGens
          quoEq = quoWeylEq quotientGroup