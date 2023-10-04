module Nat where

import Prelude hiding ((<), (+), (*), (^), quot, min, gcd, lcm, div, max, pred, rem, (-), if_then_else, leq, eq, ev, od, isMul3, divides)
import Ordering

(+) :: Nat -> Nat -> Nat
n + O = n
n + (S m) = S(n + m)

(*) :: Nat -> Nat -> Nat
n * O = O
n * (S m) = (n * m) + n

(^) :: Nat -> Nat -> Nat
n ^ O = (S O)
n ^ (S m) = n * (n ^ m)

quot :: Nat -> Nat -> Nat
quot m n = quot' m n n 
  where
    quot' :: Nat -> Nat -> Nat -> Nat
    quot' O O k = S O
    quot' O m k = O
    quot' m O k = S (quot' m k k)
    quot' (S n) (S m) k = quot' n m k
  
min :: Nat -> Nat -> Nat
min _ _ = O 
min (S m) (S n) = S (min m n)

gcd :: Nat -> Nat -> Nat
gcd n O = n 
gcd n m = gcd m (rem n m)

lcm :: Nat -> Nat -> (Nat, Nat)
lcm n m = div (n * m) (gcd n m)

div :: Nat -> Nat -> (Nat, Nat)
div n m = (quot n m, rem n m)

max :: Nat -> Nat -> Nat
max _ _ = O 
max (S n) (S m) = S (max n m)

pred :: Nat -> Nat
pred O = O
pred (S n) = n 

rem :: Nat -> Nat -> Nat
rem O n = O
rem (S m) O = S(rem m O)
rem m n = rem' m(n * (quot m n))
  where
    rem' :: Nat -> Nat -> Nat
    rem' (S m) (S n) = rem' m n 
    rem' m O = m

-- bonus 
(-) :: Nat -> Nat -> Nat
(-) n O = n 
(-) n (S m) =  n - m

fib :: Nat -> Nat
fib O = O
fib (S O) = (S O)
fib (S (S n)) = (fib (S n)) + (fib n) 

fact :: Nat -> Nat 
fact O = (S O)
fact (S n) = (S n) * (fact n)

double :: Nat -> Nat 
double O = O
double (S n) = S(S(double n))

if_then_else :: Bool -> Nat -> Nat -> Nat
if_then_else True n _ = n
if_then_else False _ m = m

eq :: Nat-> Nat -> Bool
eq O O = True
eq (S n) (S m) = eq n m
eq _ _ = False

leq :: Nat -> Nat -> Bool
leq O _ = True
leq _ O = False
leq (S n) (S m) = leq n m

ev :: Nat -> Bool
ev O = True
ev (S O) = False
ev (S (S n)) = ev n

od :: Nat -> Bool
od O = False
od (S O) = True 
od (S (S n)) = od n

isMul3 :: Nat -> Bool
isMul3 O = True
isMul3 (S O) = False
isMul3 (S (S O)) = False
isMul3 (S (S (S n))) = isMul3 n

isZero :: Nat -> Bool
isZero O = True
isZero _ = False

divides ::  Nat -> Nat -> Bool
divides O O = True
divides O (S n) = False
divides m n = eq (rem m n) O