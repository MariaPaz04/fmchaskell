module Ordering where

import Prelude hiding ((<), (>), compare)

data Nat = O | S Nat
    deriving (Eq, Show)

(<) :: Nat -> Nat -> Bool
n < m = compare n m == LT

(>=) :: Nat -> Nat -> Bool
n >= m = n > m || n == m 

(>) :: Nat -> Nat -> Bool
n > m = compare n m == GT

(<=) :: Nat -> Nat -> Bool
n <= m = n < m || n == m

compare :: Nat -> Nat -> Ordering
compare O O = EQ
compare _ O = GT
compare O _ = LT
compare (S n) (S m) = compare n m
