{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rational where

import Data.Ratio
import Test.QuickCheck.Arbitrary


newtype Rational = Rational (Ratio Integer) deriving(Eq,Ord,Num,Fractional)

instance Show Rational.Rational where
    show (Rational q) = show q

type QQ = Rational.Rational



instance Arbitrary Rational.Rational where
    arbitrary = do a <- arbitrary
                   b <- arbitrary
                   let num = (a `mod` 20) - 10
                       denom' = b `mod` 10
                       denom = if denom'==0 then 1 else denom'
                   return $ Rational.Rational (num % denom)


