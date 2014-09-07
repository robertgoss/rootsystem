{-# LANGUAGE MultiParamTypeClasses #-}
module E7 where

import E8
import RootSystem
import Weyl
import CartanAlgebra
import SubGroup
import Quotient

data E7System = E7System
data E7Weyl = E7Weyl

data E7SubE6 = E7SubE6
data E7Spin14Quotient = E7Spin14Quotient


instance RootSystem E7System E8Root where
    generators _ = init $ RootSystem.generators E8System
    rank _ = 7
    cartanAlgebra _ = CartanAlgebra.span $ map coroot $ RootSystem.generators E7System


instance WeylGroup E7Weyl E8WeylElement E7System E8Root where
    generators _ = init $ Weyl.generators E8Weyl
    one _ = one E8Weyl
    weylGroup _ = E7Weyl


instance QuotientWeylGroup E7Spin14Quotient E7Weyl E8WeylElement E7System E8Root where
    superGroup _ = E7Weyl
    quoWeylEq _ = quoWeylEq E8Spin16Quotient

instance SubWeylGroup E7SubE6 E7Weyl E8WeylElement E7System E8Root where
    ambientGroup _ = E7Weyl
    subGenerators _ = init $ Weyl.generators E7Weyl