module Thesis where

import RootSystem
import SubSystem

import Weyl
import SubGroup
import Quotient

import Spin
import Signs

import E8
import E7

import SemiSimple

import Generate

remDup [] = []
remDup (x:xs) | x `elem` xs = remDup xs
              | otherwise = x : remDup xs 

e8Group = E8Weyl

s3e7Group = subGroup E8SubS3E7

spin16QuotientGroup = Quo E8Spin16Quotient

spin16OneS3E7 = generatePointed multiply quoS3E7Gens one'
	where quoS3E7Gens = map (image E8Spin16Quotient) $ Weyl.generators s3e7Group
	      one' = one spin16QuotientGroup

s :: E8WeylElement
s = simpleReflection $ E8SRoot $ Signs.identity 8

x8 = swap23 `multiply` signSwap23
	where swap23 = simpleReflection . E8SpinRoot $ SwapRoot 2 3
	      signSwap23 = simpleReflection . E8SpinRoot $ SignSwapRoot 2 3

x8s = x8 `multiply` s 

spin16X8SS3E7 = generatePointed multiply quoS3E7Gens x8s'
	where quoS3E7Gens = map (image E8Spin16Quotient) $ Weyl.generators s3e7Group
	      x8s' = image E8Spin16Quotient x8s

commute a b = a `multiply` b == b `multiply` a

commute2 a b = (a `multiply` b) `multiply` (a `multiply` b) == b `multiply` a