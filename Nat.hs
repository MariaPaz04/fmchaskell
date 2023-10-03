module Nat where

import Prelude hiding ((-), last, (<), (>), (>=), (<=), compare, init, isPrefixOf, maximum, minimum, drop, take, enumFromTo, reverse, (++), product, sum, elem, length, (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem)
import Data
import Bool

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

gcd :: Nat -> Nat -> Nat
gcd n O = n 
gcd n m = gcd m (rem n m)

lcm :: Nat -> Nat -> (Nat, Nat)
lcm n m = div (mult n m) (gcd n m)

div :: Nat -> Nat -> (Nat, Nat)
div n m = (quot n m, rem n m)

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
minus n (S m) = minus n m

fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S (S n)) = sum (fib (S n)) (fib n) 

fact :: Nat -> Nat 
fact O = (S O)
fact (S n) = mult (S n) (fact n)

double :: Nat -> Nat 
double O = O
double (S n) = S(S(double n))