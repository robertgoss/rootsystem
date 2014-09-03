module Permutation where

import Data.Matrix
import Test.QuickCheck.Arbitrary

import Rational

newtype Permutation = Perm (Matrix Int) deriving(Eq,Show)

identity :: Int -> Permutation
identity n = Perm $ fromList n 1 [1..n]

combine :: Permutation -> Permutation -> Permutation
combine p@(Perm v) q@(Perm w) | n==m = Perm $ fromList (length new) 1 new
                   | n<m = combine (Perm $ v <-> fromList (m-n) 1 [(n+1)..m]) q
                   | otherwise = combine p (Perm $ w <-> fromList (n-m) 1 [(m+1)..n])
    where n = nrows v
          m = nrows w
          vList = toList v
          new = map ((flip at) q) vList


at :: Int -> Permutation -> Int
at n (Perm v) | (nrows v) <= n = getElem n 1 v
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


instance Arbitrary Permutation where
    arbitrary = do swaps' <- arbitrary
                   let swaps = map (\(i,j) -> ((i `mod` 9)+ 1, (j `mod` 9) + 1)) swaps'
                   return $ foldl swapTup (Permutation.identity 1) swaps
         where swapTup perm (i,j) = swap i j perm