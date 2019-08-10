{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Tools where

import Control.Monad.State
import Data.Maybe
import Data.Map
import Data.Word

getModify :: MonadState m =>
	(StateType m -> a) -> (StateType m -> StateType m) -> m a
getModify g m = gets g <* modify m

indexOrEmpty :: Ord k => Map k [v] -> k -> [v]
indexOrEmpty = (fromMaybe [] .) . (!?)

(!!?) :: Ord k => Map k [v] -> k -> Maybe v
m !!? k = join $ listToMaybe <$> m !? k

insertPush :: Ord k => k -> v -> Map k [v] -> Map k [v]
insertPush k v m = insert k (init vs ++ [v]) m
	where vs = fromMaybe [] $ m !? k

log2 :: (Integral n, Integral m) => n -> m
log2 i = i2 0
	where i2 j
		| j < 0 = error "circuit-adornt.Tools.log2.i2 j | j < 0"
		| 2 ^ j >= i = j
		| otherwise = i2 $ j + 1

binary :: (a, a) -> Word16 -> [[a]]
binary _ n | n < 1 = [[]]
binary (o, i) n = binary (o, i) (n - 1) >>= (<$> [(o :), (i :)]) . flip ($)

listToTuple2 :: [a] -> (a, a)
listToTuple2 [x, y] = (x, y)
listToTuple2 _ = error "circuit-adornt.Tools.listToTuple2 _"

listToTuple3 :: [a] -> (a, a, a)
listToTuple3 [x, y, z] = (x, y, z)
listToTuple3 _ = error "circuit-adornt.Tools.listToTuple3 _"

listToTuple4 :: [a] -> (a, a, a, a)
listToTuple4 [x, y, z, w] = (x, y, z, w)
listToTuple4 _ = error "circuit-adornt.Tools.listToTuple4 _"

unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 [] = ([], [], [], [])
unzip4 ((x, y, z, w) : xyzws) = (x : xs, y : ys, z : zs, w : ws)
	where (xs, ys, zs, ws) = unzip4 xyzws
