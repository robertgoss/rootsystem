module Generate where


import Data.Set as Set

generate :: (Ord a) => (a -> a -> a) -> [a] -> [a]
generate comb gens = toList $ generateStep genSet genSet
    where generateStep old current | Set.null current = old
                                   | otherwise = generateStep (union old new) (difference new old)
            where new = Set.unions $ Prelude.map (act current) gens
                  act x g = Set.map (comb g) x
          genSet = fromList gens

generateScan :: (Ord a) => (a -> a -> a) -> [a] -> [([a],[a])]
generateScan comb gens = generateStep [] genSet genSet
    where generateStep past old current | Set.null current = past
                                        | otherwise = generateStep ((toList old,toList new):past) (union old new) (difference new old)
            where new = Set.unions $ Prelude.map (act current) gens
                  act x g = Set.map (comb g) x
          genSet = fromList gens
