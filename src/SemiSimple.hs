module SemiSimple where

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
canonicalForm (Product semis) = Product (torus:simples)
    where flatten [] = []
          flatten ((Product xs):ys) = flatten xs ++ flatten ys
          torus = Torus (torusRank semis)
          nonTrivial = filter (not . isTrivial) (flatten semis)
          simples = map canonicalForm $ filter isSimple nonTrivial


instance Eq SemiSimple where
    x == y = case canonicalForm x,canonicalForm y of
                (A n),(A m) -> n==m
                (B n),(B m) -> n==m
                (C n),(C m) -> n==m
                (D n),(D m) -> n==m
                G2,G2 -> True
                F4,F4 -> True
                E6,E6 -> True
                E7,E7 -> True
                E8,E8 -> True
                (Torus n),(Torus m) -> m==n
                Trivial,Trivial = True
                (Product xs),(Product ys) -> xs == ys