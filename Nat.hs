module Nat where

import Prelude hiding (sum, mult, exp, quot, min, gcd, lcm, div, max, pred, rem)

data Nat = O | S Nat
    deriving ( Eq , Show )

sum :: Nat -> Nat -> Nat
sum n O = n
sum n (S m) = S(sum n m)

mult :: Nat -> Nat -> Nat
mult n O = O
mult n (S m) = sum n (mult n m)

exp :: Nat -> Nat -> Nat
exp n O = (S O)
exp n (S m) = mult n (exp n m)

quot :: Nat -> Nat -> Nat
quot m n = quot' m n n 
  where
    quot' :: Nat -> Nat -> Nat -> Nat
    quot' O O k = S O
    quot' O m k = O
    quot' m O k = S (quot' m k k)
    quot' (S n) (S m) k = quot' n m k

min :: Nat -> Nat -> Nat
min _ n = n 
min n _ = n 
min (S n) (S m) = S(min n m)

max :: Nat -> Nat -> Nat
max n _ = n 
max _ n = n 
max (S n) (S m) = S(max n m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n 

rem :: Nat -> Nat -> Nat
rem O n = O
rem (S m) O = S(rem m O)
rem m n = rem' m(mult n (quot m n))
  where
    rem' :: Nat -> Nat -> Nat
    rem' (S m) (S n) = rem' m n 
    rem' m O = m

-- bonus 
minus :: Nat -> Nat -> Nat
minus n O = n 
minus n (S m) = pred(minus n m)
