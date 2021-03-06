module Permutation where

import Data.List(sort)
import Data.Matrix
import Test.QuickCheck.Arbitrary

import Rational

newtype Permutation = Perm (Matrix Int) deriving(Show)

instance Eq Permutation where
    p@(Perm v) == q@(Perm w) | m == n = v == w
                             | m < n = pad n p == q
                             | otherwise = p == pad m q
      where m = nrows v
            n = nrows w

instance Ord Permutation where
    p@(Perm v) `compare` q@(Perm w) | m == n = toList v `compare` toList w
                                    | m < n = pad n p `compare` q
                                    | otherwise = p `compare` pad m q
      where m = nrows v
            n = nrows w

identity :: Int -> Permutation
identity n = Perm $ fromList n 1 [1..n]

combine :: Permutation -> Permutation -> Permutation
combine p@(Perm v) q@(Perm w) | n==m = Perm $ fromList (length new) 1 new
                   | n<m = combine (pad m p) q
                   | otherwise = combine p (pad n q)
    where n = nrows v
          m = nrows w
          wList = toList w
          new = map ((flip at) p) wList


at :: Int -> Permutation -> Int
at n (Perm v) | n <= (nrows v) = getElem n 1 v
              | otherwise = n

swap :: Int -> Int -> Permutation -> Permutation
swap i j (Perm v) | m <= n = Perm $ switchRows i j v
                  | otherwise = swap i j (Perm $ v <-> fromList (m-n) 1 [(n+1)..m])
    where n = nrows v
          m = max i j

toMatrix :: Permutation -> Matrix QQ
toMatrix (Perm v) = fromLists [[if i == (getElem j 1 v) then 1 else 0 | i <-[1..n]] | j <-[1..n]]
    where n = nrows v

pad :: Int -> Permutation -> Permutation
pad k p@(Perm v) | n >= k = p
                 | otherwise = Perm $ v <-> fromList (k-n) 1 [(n+1)..k]
    where n = nrows v

inverse :: Permutation -> Permutation
inverse (Perm v) = Perm $ fromList n 1 $ iList
    where assList = zip [1..] $ toList v
          iAssList = map (\(a,b) -> (b,a)) assList
          iList = map snd $ sort iAssList
          n = nrows v


instance Arbitrary Permutation where
    arbitrary = do swaps' <- arbitrary
                   let swaps = map (\(i,j) -> ((i `mod` 8)+ 1, (j `mod` 8) + 1)) swaps'
                   return $ foldl swapTup (Permutation.identity 1) swaps
         where swapTup perm (i,j) = swap i j perm