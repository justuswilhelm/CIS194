module Week4.Homework where

import qualified Data.Set

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise =  fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' =
    product . map (\s -> s - 2) . filter even


next :: Integer -> Integer
next a = if even a then a `div` 2 else 3*a+1

fun2' :: Integer -> Integer
fun2' =
    sum . takeWhile (/= 1) . iterate next

data Tree a
    = Leaf
    | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

left :: [a] -> [a]
left [] = []
left [x] = [x]
left (x:y:rest) = x:left rest

right :: [a] -> [a]
right (x:y:rest) = y:right rest
right _ = []

foldTree :: [a] -> Tree a
foldTree =
    foldr (
        (\ (Node _ _ value _) t -> Node 0 t value Leaf)
        . (\a -> Node 0 Leaf a Leaf)
    ) Leaf
    -- Node
    -- acc (foldTree_ (acc + 1) $ left xs)
    -- x
    -- (foldTree_ (acc + 1) $ right xs)

data Balanced = Balanced deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = 0
height (Node _ a _ c) = maximum [1 + height a, 1 + height c]

balanced :: Tree a -> Maybe Balanced
balanced Leaf =
    Just Balanced
balanced (Node _ a _ c) =
    if abs (height a - height c) <= 1 then
        Just Balanced
    else
        Nothing


xor :: [Bool] -> Bool
xor =
    odd . length . filter id


map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\s rest -> f s:rest) []

sequences :: Integer -> [(Integer, Integer)]
sequences n =
    [ (i, j) | i <- [1..n], j <- [1..n]]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
    let
        numbers =
            Data.Set.fromList .
            map (\(i, j) -> i + j + 2 * i * j) .
            filter (\(i, j) -> (1 <= i) && (i <= j) && (i + j + 2 * i * j <= n)) $
            sequences n
        member k =
            Data.Set.member k numbers
    in
        2:map (\s -> s * 2 + 1) (filter (not . member) [1..n])
