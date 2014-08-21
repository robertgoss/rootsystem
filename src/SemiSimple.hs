module SemiSimple where

import Test.QuickCheck.Arbitrary
import Control.Monad

import Rational
import RootSystem
import Weyl
import CartanAlgebra
import Components

import Data.List(sort)

import qualified Data.Matrix as M
import qualified Data.Map as Map

data Simple = A Int
            | B Int
            | C Int
            | D Int
            | G2 | F4 | E6 | E7 | E8 deriving(Eq,Ord,Show)

data SemiSimple = SemiSimple Int [Simple] deriving(Eq,Ord,Show)

fromSimples :: Int -> [Simple] -> SemiSimple
fromSimples torus simples = SemiSimple (torus+d2Count) (sort reducedSimples)
    where nonTrivialSimples = filter (\s -> not (isTrivial s) && s /= D 1) simples
          d2Count = length $ filter (==D 1) simples
          exceptional (C 1) = [A 1]
          exceptional (B 1) = [A 1]
          exceptional (C 2) = [B 2]
          exceptional (D 2) = [A 1,A 1]
          exceptional (D 3) = [A 6]
          exceptional x = [x]
          reducedSimples = concatMap exceptional nonTrivialSimples

isTrivial :: Simple -> Bool
isTrivial (A 0) = True
isTrivial (B 0) = True
isTrivial (C 0) = True
isTrivial (D 0) = True
isTrivial _ = False



product :: SemiSimple -> SemiSimple -> SemiSimple
product (SemiSimple torus1 simples1) (SemiSimple torus2 simples2) = fromSimples (torus1+torus2) (simples1++simples2)

dimSimple :: Simple -> Int
dimSimple (A n) = n*n + 2*n
dimSimple (B n) = n * (2*n + 1)
dimSimple (C n) = n * (2*n + 1)
dimSimple (D n) = n * (2*n - 1)
dimSimple G2 = 14
dimSimple F4 = 52
dimSimple E6 = 78
dimSimple E7 = 133
dimSimple E8 = 248

dim :: SemiSimple -> Int
dim (SemiSimple torus simples) = torus + sum (map dimSimple simples)

rankSimple :: Simple -> Int
rankSimple (A n) = n
rankSimple (B n) = n
rankSimple (C n) = n
rankSimple (D n) = n
rankSimple G2 = 2
rankSimple F4 = 4
rankSimple E6 = 6
rankSimple E7 = 7
rankSimple E8 = 8

rank :: SemiSimple -> Int
rank (SemiSimple torus simples) = torus + sum (map rankSimple simples)


rootSystemSimple :: Simple -> BasicRootSystem
rootSystemSimple (A 0) = trivialSystem
rootSystemSimple (B 0) = trivialSystem
rootSystemSimple (C 0) = trivialSystem
rootSystemSimple (D 0) = trivialSystem
rootSystemSimple (A n) = fromRoots $ map (BasicRoot . swapRoot (n+1)) [1..n]
    where swapRoot m i = M.setElem 1 (1,i) $ M.setElem (-1) (1,i+1) $ M.zero 1 m
rootSystemSimple (B n) = fromRoots $ negRoot : aRoots
    where (BasicRootSystem _ aRoots) = rootSystemSimple (A (n-1))
          negRoot = BasicRoot $ M.setElem 1 (1,1) $ M.zero 1 n
rootSystemSimple (C n) = fromRoots $ negLongRoot : aRoots
    where (BasicRootSystem _ aRoots) = rootSystemSimple (A (n-1))
          negLongRoot = BasicRoot $ M.setElem 2 (1,1) $ M.zero 1 n
rootSystemSimple (D 1) = torus . fullSubAlgebra $ 1
rootSystemSimple (D n) = fromRoots $ negSwapRoot : aRoots
    where (BasicRootSystem _ aRoots) = rootSystemSimple (A (n-1))
          negSwapRoot = BasicRoot $ M.setElem 1 (1,1) $ M.setElem 1 (1,2) $ M.zero 1 n
rootSystemSimple G2 = fromRoots $ map (BasicRoot . M.fromList 1 3) $ [[0,1,-1] , [1,-2,1]]
rootSystemSimple F4 = fromRoots $ map (BasicRoot . M.fromList 1 4) $ [[0,1,-1,0],
                                                                      [0,0,1,-1],
                                                                      [0,0,0,1],
                                                                      [1/2,-1/2,-1/2,-1/2]]
rootSystemSimple E8 = fromRoots $ excepRoot : spin14Roots'
    where (BasicRootSystem _ spin14Roots) = rootSystemSimple (D 7)
          spin14Roots' = map (BasicRoot . (M.extendTo 0 1 8) . coroot) spin14Roots
          excepRoot = BasicRoot $ (M.fromList 1 8) $ replicate 8 (-1/2)
rootSystemSimple E7 = fromRoots $ take 7 e8roots
    where (BasicRootSystem _ e8roots) = rootSystemSimple E8
rootSystemSimple E6 = fromRoots $ take 6 e8roots
    where (BasicRootSystem _ e8roots) = rootSystemSimple E8



rootSystem :: SemiSimple -> BasicRootSystem
rootSystem (SemiSimple torusDim simples) = foldl basicSystemProduct torusSystem simpleSystems
    where torusSystem = torus $ fullSubAlgebra torusDim
          simpleSystems = map rootSystemSimple simples


bondNum (a,b) = 4*(dot a b * dot b a) / (dot a a * dot b b)

determine :: (RootSystem r) => r -> SemiSimple
determine system = fromSimples torusPart simpleParts
    where connectedComps = components connected $ simpleRoots system
          connected r1 r2 = dot r1 r2 /= 0
          simpleParts = map determineSimple connectedComps
          simpleRank = sum $ map rankSimple simpleParts
          torusPart = RootSystem.rank system - simpleRank
          determineSimple roots
                | 3 `Map.member` bondCounts = G2
                | 2 `Map.member` bondCounts = determineBCF roots
                | 1 `Map.notMember` bondCounts = A 1
                | otherwise = determineADE roots
              where bondCounts = Map.fromListWith (+) $ zip bonds $ repeat 1
                    bonds = map bondNum [(r1,r2) | r1<-roots, r2<-roots , r1 < r2]
                    rank = length roots
          determineBCF roots
                | longCount == 2 && shortCount==2 = F4
                | longCount == 1 = C rank
                | shortCount == 1 = B rank
              where lengths = map (\a->dot a a) roots
                    longest = maximum lengths
                    longCount = length $ filter (==longest) lengths
                    shortCount = rank - longCount
                    rank = length roots
          determineADE roots
              | null tripleVertices = A rank
              | endToTripleNum == 2 = D rank
              | rank < 6 = D rank
              | rank==6 = E6
              | rank==7 = E7
              | rank==8 = E8
              where edges = map fst $ filter (uncurry connected) [(r1,r2) | r1<-roots, r2<-roots, r1 /= r2]
                    edgeCounts = Map.fromListWith (+) $ zip edges $ repeat 1
                    tripleVertices = Map.keys $ Map.filter (==3) edgeCounts
                    tripleVertex = head tripleVertices
                    endVertices = Map.keys $ Map.filter (==1) edgeCounts
                    endToTripleNum = length $ filter (connected tripleVertex) endVertices
                    rank = length roots

factorial :: Int -> Integer
factorial n = Prelude.product $ map toInteger [1..n]

weylOrderSimple :: Simple -> Integer
weylOrderSimple (A n) = factorial (n+1)
weylOrderSimple (B n) = 2^n * factorial n
weylOrderSimple (C n) = 2^n * factorial n
weylOrderSimple (D n) = 2^(n-1) * factorial n
weylOrderSimple G2 = 12
weylOrderSimple F4 = 1152
weylOrderSimple E6 = 51840
weylOrderSimple E7 = 2903040
weylOrderSimple E8 = 696729600

weylOrder :: SemiSimple -> Integer
weylOrder (SemiSimple torus semis) = Prelude.product $ map weylOrderSimple semis

weylGroup :: SemiSimple -> BasicWeylGroup
weylGroup = Weyl.weylGroup . rootSystem

instance Arbitrary Simple where
    arbitrary = do n <- arbitrary
                   m <- liftM (`mod` 8) arbitrary
                   let types = [A m,B m,C m,D m,G2,F4,E6,E7,E8]
                   return $ types !! (n `mod` 9)

instance Arbitrary SemiSimple where
    arbitrary = do n <- arbitrary
                   m <- arbitrary
                   xs <- arbitrary
                   return $ fromSimples (m `mod` 8) (take (n `mod` 3) xs)