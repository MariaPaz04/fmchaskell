module Ordering where

import Prelude hiding ((<), (>))
import Nat

(<) :: Nat -> Nat -> Bool
n < m = compare n m == LT

(>) :: Nat -> Nat -> Bool
n > m = compare n m == GT

(>=) :: Nat -> Nat -> Bool
n >= m = n > m || n == m 

(<=) :: Nat -> Nat -> Bool
n <= m = n < m || n == m

