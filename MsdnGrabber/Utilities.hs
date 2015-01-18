module MsdnGrabber.Utilities where

import qualified Data.Set as Set
import qualified Data.Text as T

import qualified System.FilePath.Posix as Posix

groupBy :: (a -> Bool) -> [a] -> [(a, [a])]
groupBy p elems = go $ dropWhile (not . p) elems
    where go [] = []
          go (x:xs) = (x, group) : go rest where (group, rest) = break p xs

groupBy2 :: [a] -> [(a, a)]
groupBy2 [] = []
groupBy2 (x:y:xs) = (x, y) : groupBy2 xs
groupBy2 [_] = error "Odd number of elements!"

condMap :: (a -> Bool) -> ([a] -> b) -> (a -> b) -> [a] -> [b]
condMap p f g [] = []
condMap p f g (x:xs) = if p x then g x : condMap p f g xs else f as : condMap p f g bs
    where (as, bs) = break p (x:xs)

mapTuple :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
mapTuple f g (x, y) = (f x, g y)

distinct :: Ord a => [a] -> [a]
distinct = go Set.empty where
    go _ [] = []
    go s (x : xs) = if Set.member x s
        then go s xs
        else x : go (Set.insert x s) xs

takeBaseName :: T.Text -> T.Text
takeBaseName = T.pack . Posix.takeBaseName . T.unpack

takeFileName :: T.Text -> T.Text
takeFileName = T.pack . Posix.takeFileName . T.unpack
