module SemiSimple where

import RootSystem
import Weyl

import qualified Data.Matrix as M
import Data.Ratio

data SemiSimple = Torus Int
                | Trivial
                | A Int
                | B Int
                | C Int
                | D Int
                | G2 | F4 | E6 | E7 | E8
                | Product [SemiSimple]

isTrivial :: SemiSimple -> Bool
isTrivial (Torus 0) = True
isTrivial (A 0) = True
isTrivial (B 0) = True
isTrivial (C 0) = True
isTrivial (D 0) = True
isTrivial (Product xs) = all isTrivial xs

isSimple :: SemiSimple -> Bool
isSimple (Torus n) = False
isSimple (D 1) = False
isSimple (Product []) = True
isSimple (Product (x:xs)) = isTrivial x && isSimple (Product xs) || isSimple x && all isTrivial xs
isSimple _ = True

torusRank :: SemiSimple -> Int
torusRank (Torus n) = n
torusRank (D 1) = 1
torusRank (Product xs) = sum $ map torusRank xs
torusRank _ = 0

canonicalForm :: SemiSimple -> SemiSimple
-- Zero rank algebras trivial
canonicalForm (A 0) = Trivial
canonicalForm (B 0) = Trivial
canonicalForm (C 0) = Trivial
canonicalForm (D 0) = Trivial
canonicalForm (Torus 0) = Trivial
--Exceptional isomorphisms - convert to correct spin group
canonicalForm (A 1) = (B 1)  -- SU2 == Spin3
canonicalForm (A 3) = (D 3)  -- SU4 == Spin6
canonicalForm (C 1) = (B 1)  -- Sp1 == Spin3
canonicalForm (C 2) = (B 2)  -- Sp2 == Spin5
canonicalForm (D 1) = Torus 1 --Spin2 == S^1
--Normalize products
--Flatten products - pull all torus elements together and remove trivial elements
canonicalForm product@(Product semis)
    | null simples && isTrivial torus = Trivial
    | null simples = torus
    | isTrivial torus = Product simples
    | otherwise = Product (torus:simples)
    where flatten [] = []
          flatten ((Product xs):ys) = flatten xs ++ flatten ys
          torus = Torus (torusRank product)
          nonTrivial = filter (not . isTrivial) (flatten semis)
          simples = map canonicalForm $ filter isSimple nonTrivial


instance Eq SemiSimple where
    x == y = case (canonicalForm x,canonicalForm y) of
                ((A n),(A m)) -> n==m
                ((B n),(B m)) -> n==m
                ((C n),(C m)) -> n==m
                ((D n),(D m)) -> n==m
                (G2,G2) -> True
                (F4,F4) -> True
                (E6,E6) -> True
                (E7,E7) -> True
                (E8,E8) -> True
                ((Torus n),(Torus m)) -> m==n
                (Trivial,Trivial) -> True
                ((Product xs),(Product ys)) -> xs == ys

singleRoot m i = BasicRoot $ M.setElem 1 (1,i) $ M.zero 1 m
singleDoubleRoot m i = BasicRoot $ M.setElem 2 (1,i) $ M.zero 1 m
swapRoot m i = BasicRoot $ M.setElem (-1) (1,i+1) $ M.setElem 1 (i,1) $ M.zero 1 m
swapNegRoot m i = BasicRoot $ M.setElem 1 (1,i+1) $ M.setElem 1 (i,1) $ M.zero 1 m

rootSystem :: SemiSimple -> BasicRootSystem
rootSystem (A n) = BasicRootSystem $ map (swapRoot (n+1)) [1..(n-1)]
rootSystem (B n) = BasicRootSystem $ singleRoot n 1 : map (swapRoot n) [1..(n-2)]
rootSystem (C n) = BasicRootSystem $ singleDoubleRoot n 1 : map (swapRoot n) [1..(n-2)]
rootSystem (D n) = BasicRootSystem $ swapNegRoot n 1 : map (swapRoot n) [1..(n-2)]
rootSystem G2 = BasicRootSystem $ map (BasicRoot . (M.fromList 1 3)) [[0,1,-1] , [1,-2,1]]
rootSystem F4 = BasicRootSystem $ map (BasicRoot . (M.fromList 1 4)) [[0,1,-1,0] , [0,0,1,-1],[0,0,0,1],[1%2,-1%2,-1%2,-1%2]]
rootSystem E8 = BasicRootSystem $ BasicRoot (M.fromList 1 8 []) : spin16
    where (BasicRootSystem spin16) = rootSystem (D 8)
rootSystem E7 = BasicRootSystem $ drop 1 e8
    where (BasicRootSystem e8) = rootSystem E8
rootSystem E6 = BasicRootSystem $ drop 2 e8
    where (BasicRootSystem e8) = rootSystem E8
rootSystem Trivial = BasicRootSystem []
rootSystem (Torus n) = BasicRootSystem []
rootSystem (Product semis) = BasicRootSystem $ concat $ zipWith3 pad subSystems leftPad rightPad
    where leftPad = scanl (+) 0 $ map basicDim subSystems
          rightPad = tail $ scanl (-) total $ map basicDim subSystems
          total = sum $ map basicDim subSystems
          pad (BasicRootSystem roots) i j = map (pad' i j) roots
          pad' i j (BasicRoot root) = BasicRoot $ M.zero 1 i M.<|> root M.<|> M.zero 1 j
          subSystems = map rootSystem semis