module Signs where

import Data.Matrix
import Test.QuickCheck.Arbitrary

import Rational
import qualified Permutation as P

newtype Signs = Signs (Matrix Int) deriving (Eq,Show)

identity :: Int -> Signs
identity n = Signs $ fromList n 1 $ replicate n 1

pad :: Int -> Signs -> Signs
pad k s@(Signs v) | k <= n = s
                  | otherwise = Signs $ v <-> fromList (k-n) 1 (replicate (k-n) 1)
    where n = nrows v

combine :: Signs -> Signs -> Signs
combine s@(Signs v) t@(Signs w) | m == n = Signs $ fromList m 1 $ zipWith (*) (toList v) (toList w)
                                | m < n = combine (pad n s) t
                                | otherwise = combine s (pad m t)
    where m = nrows v
          n = nrows w

dSwap :: Int -> Int -> Signs -> Signs
dSwap i j s@(Signs v) | m <= n = Signs $ setElem (-vi) (i,1) $ setElem (-vj) (j,1) $ v
                      | otherwise = dSwap i j (pad m s)
    where m = max i j
          n = nrows v
          vi = getElem i 1 v
          vj = getElem j 1 v

at :: Int -> Signs -> Int
at i (Signs v) | nrows v >= i = getElem i 1 v
               | otherwise = 1

permute :: P.Permutation -> Signs -> Signs
permute p@(P.Perm v) s@(Signs w) | n == m = Signs $ fromList m 1 $ map ((flip at) s) vList
                                 | n > m = permute (P.pad m p) s
                                 | otherwise = permute p (pad n s)
    where vList = toList v
          m = nrows v
          n = nrows w

toMatrix :: Signs -> Matrix QQ
toMatrix s@(Signs v) = fromLists [[if i==j then fromIntegral (at i s) else 0 | i<-[1..n] ] | j<-[1..n]]
    where n= nrows v

instance Arbitrary Signs where
    arbitrary = do swaps' <- arbitrary
                   let swaps = map (\(i,j) -> ((i `mod` 9)+ 1, (j `mod` 9) + 1)) swaps'
                   return $ foldl swapTup (Signs.identity 1) swaps
         where swapTup perm (i,j) = dSwap i j perm