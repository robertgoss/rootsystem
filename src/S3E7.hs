{-# LANGUAGE MultiParamTypeClasses #-}
module S3E7 where

import E8
import RootSystem
import Weyl
import CartanAlgebra
import SubGroup
import Quotient
import Spin
import SubSystem

data S3E7System = S3E7System

data S3E7Weyl = S3E7Weyl

data S3E7SubS3S1E6 = S3E7SubS3S1E6
data S3E7Spin4Spin12Quotient = S3E7Spin4Spin12Quotient
data S3E7SubS3S1E6System = S3E7SubS3S1E6System
data S3E7SubSpin4Spin12System = S3E7SubSpin4Spin12System


instance RootSystem S3E7System E8Root where
    generators _ = s3Root : init (RootSystem.generators E8System)
        where s3Root = E8SpinRoot $ SwapRoot 7 8 
    rank _ = 8
    cartanAlgebra _ = fullSubAlgebra 8

instance SubRootSystem S3E7SubS3S1E6System S3E7System E8Root where
    ambientSystem _ = S3E7System
    subGenerators _ = init $ RootSystem.generators S3E7System

instance SubRootSystem S3E7SubSpin4Spin12System S3E7System E8Root where
    ambientSystem _ = S3E7System
    subGenerators _ = (E8SpinRoot $ SignSwapRoot 7 8) : tail (RootSystem.generators S3E7System)


instance WeylGroup S3E7Weyl E8WeylElement S3E7System E8Root where
    generators _ = Weyl.generators $ subGroup E8SubS3E7
    one _ = one E8Weyl
    weylGroup _ = S3E7Weyl


instance QuotientWeylGroup S3E7Spin4Spin12Quotient S3E7Weyl E8WeylElement S3E7System E8Root where
    superGroup _ = S3E7Weyl
    quoWeylEq _ = quoWeylEq E8Spin16Quotient

instance QuotientWeylGroupCmp S3E7Spin4Spin12Quotient S3E7Weyl E8WeylElement S3E7System E8Root where
    quoWeylCmp _ = quoWeylCmp E8Spin16Quotient

instance SubWeylGroup S3E7SubS3S1E6 S3E7Weyl E8WeylElement S3E7System E8Root where
    ambientGroup _ = S3E7Weyl
    subGenerators _ = Weyl.generators $ subGroup E8SubS3S1E6