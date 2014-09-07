module DoubleQuotient where

import Weyl
import SubGroup
import Quotient
import Generate

data DQuo a = DQuo a (a->a->Bool)

instance Eq (DQuo a) where
    (DQuo x eqFun) == (DQuo y _) = eqFun x y

preimage (DQuo a _) = a

pushforward2 :: (a -> a -> a) -> DQuo a -> DQuo a -> DQuo a
pushforward2 f (DQuo a eqFun) (DQuo b _)= DQuo (f a b) eqFun 

doubleQuotient :: (SubWeylGroup sub w e r rt, QuotientWeylGroup quo w e r rt) => sub -> quo -> [e]
doubleQuotient subGroup quotientGroup = map preimage $ generateUnOrd quoMultiply genImages
    where image elem = DQuo elem quoEq
          quoEq = quoWeylEq quotientGroup
          genImages = map image $ subGenerators subGroup
          quoMultiply = pushforward2 multiply