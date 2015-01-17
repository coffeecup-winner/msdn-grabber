module MsdnGrabber.Utilities where

import qualified Data.Set as Set

groupBy :: (a -> Bool) -> [a] -> [(a, [a])]
groupBy p elems = go $ dropWhile (not . p) elems
    where go [] = []
          go (x:xs) = (x, group) : go rest where (group, rest) = break p xs

groupBy2 :: [a] -> [(a, a)]
groupBy2 [] = []
groupBy2 (x:y:xs) = (x, y) : groupBy2 xs

mapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f g (x, y) = (f x, g y)

distinct :: Ord a => [a] -> [a]
distinct = go Set.empty where
    go _ [] = []
    go s (x : xs) = if Set.member x s
        then go s xs
        else x : go (Set.insert x s) xs
