module MsdnGrabber.Utilities where

groupBy :: (a -> Bool) -> [a] -> [(a, [a])]
groupBy p elems = go $ dropWhile (not . p) elems
    where go [] = []
          go (x:xs) = (x, group) : go rest where (group, rest) = break p xs

groupBy2 :: [a] -> [(a, a)]
groupBy2 [] = []
groupBy2 (x:y:xs) = (x, y) : groupBy2 xs

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (x, y) = (f x, f y)
