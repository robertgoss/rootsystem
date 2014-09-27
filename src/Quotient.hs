{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Quotient where

import Weyl
import SubGroup
import Generate
import RootSystem

class (WeylGroup w e r rt) => QuotientWeylGroup qw w e r rt | qw -> w, qw -> e, qw -> r, qw -> rt where
    superGroup :: qw -> w
    quoWeylEq :: qw -> e -> e -> Bool

data SubGroupQuotient s w e = SubQuo s [e]

instance (WeylGroup w e r rt, SubWeylGroup s w e r rt) 
                => QuotientWeylGroup (SubGroupQuotient s w e) w e r rt where

    superGroup (SubQuo sub _) = ambientGroup sub

    quoWeylEq (SubQuo sub subElems) g1 g2 = difference `elem` subElems
                where difference = g1 `multiply` (inverse g2)


data QuotientElement qw a = QuoE qw a

instance (QuotientWeylGroup qw w e r rt) => Eq (QuotientElement qw e) where
    (QuoE quo x) == (QuoE _ y) = (quoWeylEq quo) x y


preimage :: QuotientElement qw e -> e
preimage (QuoE _ a) = a

pushforward :: (a -> a) -> QuotientElement qw a -> QuotientElement qw a
pushforward f (QuoE quo a) = QuoE quo (f a)

pushforward2 :: (a -> a -> a) -> QuotientElement qw a -> QuotientElement qw a -> QuotientElement qw a
pushforward2 f (QuoE quo a) (QuoE _ b)= QuoE quo (f a b)

instance (QuotientWeylGroup qw w e r rt) => WeylGroupElement (QuotientElement qw e) rt where
    inverse = pushforward inverse
    multiply = pushforward2 multiply
    simpleReflection = undefined
    weylAction = weylAction . preimage

    torusRepresentation = torusRepresentation . preimage

data QuotientGroup a = Quo a

instance (QuotientWeylGroup qw w e r rt) => WeylGroup (QuotientGroup qw) (QuotientElement qw e) r rt where
  one (Quo qw) = QuoE (one $ superGroup qw) qw
  generators (Quo qw) = map (QuoE qw) $ Weyl.generators $ superGroup qw
  weylGroup = undefined