module List where
import Prelude hiding (dropWhile, takeWhile, (^), (*), (+), reverse, fold, map, (<=), (>=), compare, replicate, filter, all, any)

import Nat
import ListNat hiding (expNat, mulNat, reverse, append)
import Ordering

replicate :: Nat -> a -> [a]
replicate O _ = []
replicate (S n) x = x : replicate n x

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x : xs) = f x : map f xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (n : ns)
   | p n = n : filter p ns
   | otherwise = filter p ns

all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f (x : xs) = f x && all f xs

any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f (x : xs) = f x || all f xs

pw :: (a -> b -> c) -> [a] -> [b] -> [c]
pw f (x : xs) (y : ys) = f x y : pw f xs ys
pw _ _ _ = []

fold :: (a -> a -> a) -> a -> [a] -> a
fold _ x [] = x
fold f x (y : ys) = f y (fold f x ys)

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (n : ns)
  | p n = n : takeWhile p ns
  | otherwise = []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (n : ns)
  | p n = dropWhile p ns
  | otherwise = n : ns

addNat :: Nat -> [Nat] -> [Nat]
addNat m = map (+ m)

mulNat :: Nat -> [Nat] -> [Nat]
mulNat m = map (* m)

expNat :: Nat -> [Nat] -> [Nat]
expNat m = map (^ m)