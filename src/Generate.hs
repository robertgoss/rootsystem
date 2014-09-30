module Generate where

import Debug.Trace
import Data.Set as Set

generate :: (Ord a) => (a -> a -> a) -> [a] -> [a]
generate comb gens = toList $ generateStep genSet genSet
    where generateStep old current | Set.null current = old
                                   | otherwise = generateStep (union old new) (difference new old)
            where new = Set.unions $ Prelude.map (act current) gens
                  act x g = Set.map (comb g) x
          genSet = fromList gens

generatePointed :: (Ord a) => (a -> a -> a) -> [a] -> a -> [a]
generatePointed comb gens basePoint = toList $ generateStep baseSet genSet
    where generateStep old current | Set.null current = old
                                   | otherwise = generateStep (union old new) (difference new old)
            where new = Set.unions $ Prelude.map (act current) gens
                  act x g = Set.map (comb g) x
          genSet = fromList gens
          baseSet = fromList [basePoint]

generateScan :: (Ord a) => (a -> a -> a) -> [a] -> [([a],[a])]
generateScan comb gens = generateStep [] genSet genSet
    where generateStep past old current | Set.null current = past
                                        | otherwise = generateStep ((toList old,toList new):past) (union old new) (difference new old)
            where new = Set.unions $ Prelude.map (act current) gens
                  act x g = Set.map (comb g) x
          genSet = fromList gens

reduce :: (Eq a) => [a] -> [a]
reduce [] = []
reduce (x:xs) = x : reduce (Prelude.filter (/=x) xs)

unOrdUnion a b = reduce (a++b)

unOrdDifference a b = reduce $ unOrdDifference' a b

unOrdDifference' [] _ = []
unOrdDifference' (x:xs) ys | x `notElem` ys = x : unOrdDifference' xs ys
                           | otherwise = unOrdDifference' xs ys

unOrdUnions xs = Prelude.foldl unOrdUnion [] xs

generateUnOrd :: (Eq a) => (a -> a -> a) -> [a] -> [a]
generateUnOrd comb gens = generateUnOrdStep gens gens
    where generateUnOrdStep old current | Prelude.null current = old
                                        | otherwise = generateUnOrdStep (unOrdUnion old new) (unOrdDifference new old)
            where new = unOrdUnions $ Prelude.map (act current) gens
                  act x g = Prelude.map (comb g) x
