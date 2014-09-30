module Thesis where

import RootSystem
import SubSystem

import Weyl
import SubGroup
import Quotient

import Spin
import Signs

import E8
import S3E7

import SemiSimple

import Generate

remDup [] = []
remDup (x:xs) | x `elem` xs = remDup xs
              | otherwise = x : remDup xs 

e8Group = E8Weyl

s3e7Group = S3E7Weyl
s3s1e6Group = subGroup S3E7SubS3S1E6

spin16QuotientGroup = Quo E8Spin16Quotient

spin4spin12QuotientGroup = Quo S3E7Spin4Spin12Quotient

spin16OneS3E7 = generatePointed multiply quoS3E7Gens one'
	where quoS3E7Gens = map (image E8Spin16Quotient) $ Weyl.generators s3e7Group
	      one' = one spin16QuotientGroup

spin4spin12OneS3S1E6 = generatePointed multiply quoS3S1E6Gens one'
	where quoS3S1E6Gens = map (image S3E7Spin4Spin12Quotient) $ Weyl.generators s3s1e6Group
	      one' = one spin4spin12QuotientGroup

s :: E8WeylElement
s = simpleReflection $ E8SRoot $ Signs.identity 8

x8 = swap67 `multiply` signSwap67
	where swap67 = simpleReflection . E8SpinRoot $ SwapRoot 6 7
	      signSwap67 = simpleReflection . E8SpinRoot $ SignSwapRoot 6 7

x8s = x8 `multiply` s 

spin16X8SS3E7 = generatePointed multiply quoS3E7Gens x8s'
	where quoS3E7Gens = map (image E8Spin16Quotient) $ Weyl.generators s3e7Group
	      x8s' = image E8Spin16Quotient x8s

x7 = swap56 `multiply` signSwap56
	where swap56 = simpleReflection . E8SpinRoot $ SwapRoot 5 6
	      signSwap56 = simpleReflection . E8SpinRoot $ SignSwapRoot 5 6

x7s = x7 `multiply` s 

spin4spin12X7SS3S1E6 = generatePointed multiply quoS3S1E6Gens x7s'
	where quoS3S1E6Gens = map (image S3E7Spin4Spin12Quotient) $ Weyl.generators s3s1e6Group
	      x7s' = image S3E7Spin4Spin12Quotient x7s

commute a b = a `multiply` b == b `multiply` a

commute2 a b = (a `multiply` b) `multiply` (a `multiply` b) == b `multiply` a

e8System = E8System
e8SubS3E7System = E8SubS3E7System
e8SubSpin16System = E8SubSpin16System

s3E7System = S3E7System
s3E7SubS3S1E6System = S3E7SubS3S1E6System
s3E7SubSpin4Spin12System = S3E7SubSpin4Spin12System

e8DoubleQuoAtOne = intersection $ combineSubSystems e8SubS3E7System e8SubSpin16System
e8DoubleQuoAtX8S = intersection $ combineSubSystems e8SubS3E7System (actOnSubSystem e8SubSpin16System x8s)

s3E7DoubleQuoAtOne = intersection $ combineSubSystems s3E7SubS3S1E6System s3E7SubSpin4Spin12System
s3E7DoubleQuoAtX7S = intersection $ combineSubSystems s3E7SubS3S1E6System (actOnSubSystem s3E7SubSpin4Spin12System x7s)