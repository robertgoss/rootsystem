module Components where

import Data.Set as Set

components :: (Ord a) => (a -> a -> Bool) -> [a] -> [[a]]
components _ [] = []
components edge list@(x:xs) = toList component : components edge (toList rest)
    where (component,rest) = closure edge (fromList list) [x]

closure :: (Ord a) => (a -> a -> Bool) -> Set a -> [a] -> (Set a,Set a)
closure edge vertices seed = closureNext (fromList []) seed
    where closureNext old current | Prelude.null current = (old, Set.difference vertices old)
                                  | otherwise = closureNext (Set.union old new) (toList (Set.difference new old))
              where new = Set.filter connectedCurr vertices
                    connectedCurr vertex = all (edge vertex) current
