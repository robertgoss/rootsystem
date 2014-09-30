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

s :: E8WeylElement
s = simpleReflection $ E8SRoot $ Signs.identity 8

x8 = swap67 `multiply` signSwap67
	where swap67 = simpleReflection . E8SpinRoot $ SwapRoot 6 7
	      signSwap67 = simpleReflection . E8SpinRoot $ SignSwapRoot 6 7

x8s = x8 `multiply` s 

spin16X8SS3E7 = generatePointed multiply quoS3E7Gens x8s'
	where quoS3E7Gens = map (image E8Spin16Quotient) $ Weyl.generators s3e7Group
	      x8s' = image E8Spin16Quotient x8s

commute a b = a `multiply` b == b `multiply` a

commute2 a b = (a `multiply` b) `multiply` (a `multiply` b) == b `multiply` a